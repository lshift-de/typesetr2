package net.lshift.typesetr
package postprocessors

import net.lshift.typesetr.pandoc.Markers
import net.lshift.typesetr.xml.attributes.TextAlign
import parsers.odt.styles._
import styles.MetaFromDocument

import xml._
import xml.InternalTags._
import xml.Attributes._
import parsers.{ Repr, NodeFactory, DocumentStyle, NodeConfigs, NodeInfo }

import util.Logger

import scala.annotation.tailrec

import scalaz.Tags.First
import scalaz.Scalaz._

trait Optimizer[T] {

  type ElemSig = (Tag, List[Attribute])

  def optimize(node: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Repr.Aux[T] = {
    val nodes = coalesce(node.body.toList)
    implicitly[NodeFactory.Aux[T]].create(
      tag = node.tag,
      docNode = node.source,
      children = nodes,
      contents = node.contents,
      attrs = node.attr)
  }

  protected def coalesce(nodes: List[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): List[Repr.Aux[T]]

  implicit protected def nodeConfig: NodeConfigs.WithNode[T]

}

trait PostProcessorUtils[T] extends OpimizerStrategies[T] {
  self: Optimizer[T] =>

  import xml.TagGroups._

  // group together potential elements
  protected def coalesce(nodes: List[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): List[Repr.Aux[T]] = {

    object BogusElement {
      def isBogus(x: Repr): Boolean = unapply(x).isEmpty

      def unapply(e: Repr): Option[Repr] =
        if (e hasTag FULLY_VOID_TAGS)
          Some(e)
        //emptyblock element
        else if ((e hasTag NON_EMPTY_BLOCK_TAGS) && e.isEmpty)
          Some(e)
        // empty link
        else if ((e hasTag A) && (e hasAttribute HREF))
          Some(e)
        else
          None
    }

    def groupByTagAndAttr(x: Repr.Aux[T]): Int => GroupKey = (idx: Int) =>
      x.tag match {
        case Tag.textTag => TextKey(idx)
        case t =>
          t.state match {
            case Tag.Optimize => SigKey((t, x.attr), idx)
            case Tag.Leave    => SkolemKey(idx)
            case Tag.Replace  => RemoveKey(idx)
          }
      }

    def maybeCollapseGroups(key: ElemSig, elems: Seq[Repr.Aux[T]])(implicit logger: Logger): Seq[Repr.Aux[T]] = {
      elems match {
        case singleElem :: Nil => elems
        case _ =>
          logger.debug(s"Collapse groups of ${key._1}")
          key._1 match {
            case inline if inline isIn INLINE_TAG_WITH_BLOCKQUOTE =>
              // weird odt splitting
              // TODO: enable filtering once styles are back
              coalesceSiblings(key, elems) //.filterNot(BogusElement.isBogus)
            case BLOCK =>
              // collapse blocks into one
              coalesceBlocks(key, elems) //.filterNot(BogusElement.isBogus)
            // FIXME: missing LIT and CMD
            // checking
            case _ =>
              logger.debug("Coalesce based on parent-child relation")
              (for {
                elem <- elems
                elem1 <- coalesceParentChild(key, elem)
                elem2 <- coalesceHeadings(elem1)
              } yield elem2).flatten
            // TODO: looks like the current implementation
            // is too eager.
            //elems
          }
      }
    }

    val nodes1 = nodes.map(optimize(_))

    // Group all the child nodes by the tag.
    // Note: when grouping we have to consider if they are not
    //       separated by some other group, hence the
    //       additional indexing.
    val withGroupTag = nodes1.foldLeft((0, (Nil: List[GroupKey], Nil: List[Repr.Aux[T]])))({
      case ((0, (Nil, Nil)), r) =>
        (0, (groupByTagAndAttr(r)(0) :: Nil, r :: Nil))
      case ((idx, (tags @ (last :: rest), reprs)), r) =>
        val groupedMaybe = groupByTagAndAttr(r)
        val grouped = groupedMaybe(idx)
        if (last == grouped) (idx, (grouped :: tags, r :: reprs))
        else {
          val idx1 = idx + 1
          (idx1, (groupedMaybe(idx1) :: tags, r :: reprs))
        }
    })._2

    val grouped = withGroupTag._2.zip(withGroupTag._1).reverse.groupBy(_._2)

    (for {
      key <- grouped.keys.toList.sortBy(_.idx)
      elems0 <- grouped.get(key)
    } yield {
      val elems = elems0.map(_._1)
      key match {
        case TextKey(_) =>
          val text = elems.flatMap(_.extractPlainText).mkString("")
          Repr.makeTextElem(text)(
            implicitly[NodeFactory.Aux[T]].textNode(text),
            implicitly[NodeFactory.Aux[T]]) :: Nil
        case SigKey(key, _) =>
          maybeCollapseGroups(key, elems)
        case SkolemKey(_) =>
          elems
        case RemoveKey(_) =>
          Nil
      }
    }).flatten

  }

  sealed abstract class GroupKey {
    def idx: Int
  }
  case class TextKey(idx: Int) extends GroupKey
  case class SigKey(elemSig: ElemSig, idx: Int) extends GroupKey
  case class SkolemKey(idx: Int) extends GroupKey
  case class RemoveKey(idx: Int) extends GroupKey

}

trait OptimizerCoalesceBlocks[T] {
  self: Optimizer[T] =>

  import xml.TagGroups._

  // .block -> pre | blockquote handling
  // hacky and limited ATM; no support for nesting etc.

  // Note that we have to consider blocks and their children at the same time.
  //
  protected def coalesceBlocks(sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Seq[Repr.Aux[T]] = {

    @tailrec
    def codeBlock(elems: Seq[Repr.Aux[T]], codeElems: Seq[Repr.Aux[T]]): (Seq[Repr.Aux[T]], Seq[Repr.Aux[T]]) = elems match {
      case (elem) :: rest if elem hasTag CODE =>
        codeBlock(rest, elem +: codeElems)
      case _ =>
        (codeElems, elems)
    }

    @tailrec
    def extractNonCodeNodes(elems: Seq[Repr.Aux[T]], blockq: Seq[Repr.Aux[T]])(implicit blockParent: Repr.Aux[T]): (Seq[Repr.Aux[T]], Seq[Repr.Aux[T]]) = elems match {
      case (elem: Repr) :: rest if elem hasTag CODE =>
        (blockq, elems)

      case (elem: Repr) :: rest =>
        val styleId = nodeConfig.styleExtractor.extractId(blockParent)
        val align =
          for {
            id <- styleId
            style <- sty.style(id)
            textStyle <- style.textAlign
          } yield textStyle

        if (align.map(_ == TextAlign.Right).getOrElse(false)) {
          // this is a citation.
          // append footer
          extractNonCodeNodes(rest, elem +: blockq)
        } else {
          val toAppend =
            if ((elem hasTag BLOCK_TAGS) || (elem hasTag FOOTNOTE))
              // wrap in P
              Repr.makeElem(P, Seq(elem), contents = None, attrs = ???)(???, ???)
            else
              elem
          extractNonCodeNodes(rest, toAppend +: blockq)
        }

      case _ =>
        // No more blocks
        (blockq, elems)
    }

    @tailrec
    def coalesceBodiesOfBlocks(remainingElemsOfBody: Seq[Repr.Aux[T]],
                               blocks: Seq[Repr.Aux[T]],
                               currentBlock: Repr.Aux[T],
                               acc: Seq[Repr.Aux[T]], subAcc: Seq[Repr.Aux[T]]): Seq[Repr.Aux[T]] = {

      remainingElemsOfBody match {
        case Nil =>
          blocks match {
            case Nil =>
              // completely done
              val codeBlock = createCodeBlock(subAcc)(currentBlock).map(Seq(_)).getOrElse(Seq.empty)
              acc.reverse
            case head :: rest =>
              coalesceBodiesOfBlocks(head.body, rest, head, acc, subAcc)
          }

        case _ =>

          val subAcc1 = if (subAcc.isEmpty) subAcc else insertNewLine() +: subAcc
          val (codeNodes, rest1) = codeBlock(remainingElemsOfBody, subAcc1)
          if (rest1.isEmpty) {
            // Used up nodes when collecting code blocks.
            // Try the next paragraph (and its children), if possible.
            blocks match {
              case Nil =>
                // completely done.
                val codeBlock = createCodeBlock(codeNodes)(currentBlock).map(Seq(_)).getOrElse(Seq.empty)
                acc.reverse ++ codeBlock
              case head :: rest =>
                coalesceBodiesOfBlocks(head.body, rest, head, acc, codeNodes)
            }
          } else {

            // 1. Create a code block, since the analysis finished with some non-code block leftovers
            // 2. Create a non-code block (blockquote) from the remaining non-code blocks
            // 3. Append the two and recursively call the main analysis again

            val codeBlock = createCodeBlock(codeNodes)(currentBlock).map(Seq(_)).getOrElse(Seq.empty)
            val (nonCodeNodes, rest2) = extractNonCodeNodes(rest1, blockq = Nil)(currentBlock)
            val nonCodeBlock = createNonCodeBlock(nonCodeNodes)(currentBlock)
            coalesceBodiesOfBlocks(rest2, blocks, currentBlock, nonCodeBlock ++ codeBlock ++ acc, Nil)
          }
      }
    }

    def insertNewLine(): Repr.Aux[T] =
      Repr.makeElem(
        tag = Tag.nodeTag,
        body = Seq(),
        contents = None,
        attrs = Nil)(nodeConfig.nodeFactory.newLineNode(), nodeConfig.nodeFactory)

    def createCodeBlock(codeElems0: Seq[Repr.Aux[T]])(paragraphBlock: Repr.Aux[T]): Option[Repr.Aux[T]] = {
      if (codeElems0.isEmpty) None
      else
        Some(
          Repr.makeElem(
            tag = PRE,
            body = Markers.formatBlock(codeElems0.reverse),
            contents = None,
            attrs = Nil)(paragraphBlock.source, implicitly[NodeFactory.Aux[T]]))
    }

    def createNonCodeBlock(nonCodeElems0: Seq[Repr.Aux[T]])(paragraphBlock: Repr.Aux[T]): Seq[Repr.Aux[T]] = {
      val nonCodeElems = nonCodeElems0.reverse

      nonCodeElems match {
        case Nil =>
          Nil
        case all @ (head :: rest) =>
          val styleId = nodeConfig.styleExtractor.extractId(paragraphBlock)
          val align =
            for {
              id <- styleId
              style <- sty.style(id)
              textStyle <- style.textAlign
            } yield textStyle

          val formatted =
            if (align.map(_ == TextAlign.Right).getOrElse(false))
              // this is a citation.
              // append footer
              Markers.citationInBlockQuotation { all }
            else
              all

          Seq(
            Repr.makeElem(
              tag = BLOCKQUOTE,
              body = formatted,
              contents = None,
              attrs = paragraphBlock.attr)(paragraphBlock.source, implicitly[NodeFactory.Aux[T]]))
      }
    }

    coalesceBodiesOfBlocks(
      remainingElemsOfBody = Nil,
      blocks = elems,
      currentBlock = elems.head,
      acc = Nil,
      subAcc = Nil)
  }

}

trait OptimizerCoalesceSiblings[T] {
  self: Optimizer[T] =>

  protected def coalesceSiblings(sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Seq[Repr.Aux[T]] = {
    // pack together the elements of the group
    // should apply cleaning up recursively
    val compactedElems = coalesce(elems.flatMap(_.body).toList)

    logger.debug(s"coalesce siblings: $sig > Reduced ${elems.length} to ${compactedElems.length}")
    elems match {
      case Nil => Nil
      case first :: rest =>
        if (sig._1 == SPAN) compactedElems
        // TODO, create a new XML node
        else {
          Repr.makeElem(sig._1, compactedElems, contents = None,
            attrs = sig._2)(first.source, implicitly[NodeFactory.Aux[T]]) :: Nil
        }

    }
  }
}

trait OptimzerCoalesceHeadings[T] {
  self: Optimizer[T] =>

  import xml.TagGroups._

  object IsAnchor {
    def unapply(node: Repr): Option[Repr] = node match {
      case (elem: Repr) if (elem hasTag A) && (elem hasAttribute NAME) =>
        Some(elem)
      case _ =>
        None
    }
  }

  // Is there any actual textual content in the heading?
  protected def coalesceHeadings(body: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Option[Seq[Repr.Aux[T]]] = {
    implicit def source: T = body.source

    if (H_TAGS.contains(body.tag)) {

      val (cleanedBody, noImgFig) =
        (for {
          bodyElem <- whack(body, n => !(n hasTag CAN_OCCUR_IN_HEADER))
          noImgElem <- whack(bodyElem, n => n hasTag List(IMG, FIGURE))
        } yield (bodyElem, noImgElem)).unzip

      if (isBlank(noImgFig)) {
        logger.debug(s"[optimizer] empty img figure $noImgFig in $body of ${body.source}")
        val elem = Repr.makeElem(
          tag = body.tag,
          body = cleanedBody,
          contents = body.contents,
          attrs = body.attr.filter(_.key != STYLE))
        Some(elem :: Nil)
      } else {
        /*val nodes = (for {
          elem <- cleanedBody if !isAnchor(elem) && !implicitly[NodeInfo.Aux[T]].isText(elem)
        } yield elem)

        if (nodes.isEmpty) {
          logger.debug(s"[optimizer] headings: non-anchor and non-string in $cleanedBody")
          None
        } else Some(nodes)*/
        Some(body :: Nil)
      }
    } else Some(body :: Nil)
  }
}

trait OpimizerStrategies[T] {
  self: Optimizer[T] =>

  protected def coalesceBlocks(sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Seq[Repr.Aux[T]]
  protected def coalesceSiblings(sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Seq[Repr.Aux[T]]
  protected def coalesceHeadings(body: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Option[Seq[Repr.Aux[T]]]
  protected def coalesceParentChild(sig: ElemSig, elem: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Option[Repr.Aux[T]]
}

trait OptimizerCoalesceParentChild[T] {
  self: Optimizer[T] =>

  // rationale:
  //     <li>
  //       <p>a</p>
  //       <ul>...</ul>
  //     </li>
  // should be transformed to:
  //     <li>
  //       a
  //       <ul>...</ul>
  //     </li>
  protected def coalesceParentChild(sig: ElemSig, elem: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Option[Repr.Aux[T]] = {

    object BodyWithBogusP {
      val liftableTags = List(LI, DT, DD, FOOTNOTE)

      object DoesNotStartWithP {
        def unapply(elems: List[Repr.Aux[T]]) = {
          if (elems exists (_.hasTag(P))) None
          else Some(elems)
        }
      }

      def unapply(elem: Repr.Aux[T]) = {
        if (elem hasTag liftableTags)
          elem.body match {
            case (elem: Repr) :: DoesNotStartWithP(elems) if elem.hasTag(P) =>
              Some(elem.body.asInstanceOf[Seq[Repr.Aux[T]]] ++ elems)
            case _ =>
              None
          }
        else None
      }
    }

    object LiftableP {
      val allowedInnerTags = List(PAGEBREAK, BLOCKQUOTE, PRE)

      def unapply(elem: Repr.Aux[T]) = (elem, elem.body) match {
        case (elem: Repr, child :: Nil) if elem.hasTag(P) && (child hasTag allowedInnerTags) =>
          Some(child)
        case _ =>
          None
      }

    }

    object LiftableSpanStyle {
      val invalidAttributes = List(COLOR, BACKGROUND_COLOR)

      def unapply(elem: Repr.Aux[T]): Option[(Seq[Attribute], Seq[Repr.Aux[T]])] =
        elem match {
          case (elem: Repr) if (elem hasTag SPAN) && (elem hasAttribute STYLE) =>
            val attrs = elem.attr.filter { _.key in invalidAttributes }
            Some((attrs, elem.body))
          case _ =>
            None
        }

    }

    // If the span applies no interesting formatting
    // it can essentially by the body inside it
    object LiftableTextSpan {

      def unapply(elem: Repr.Aux[T]): Option[Seq[Repr.Aux[T]]] = {
        val id = nodeConfig.styleExtractor.extractId(elem)
        id flatMap (sty.style) flatMap { style =>
          elem match {
            case (elem: Repr) if (elem hasTag SPAN) && style.isReducible =>
              Some(elem.body)
            case _ =>
              None
          }
        }
      }

    }

    Some(elem match {
      case BodyWithBogusP(children) =>
        logger.debug("[parent-child] body with bogus p")
        Repr.makeElem(elem.tag, children, attrs = elem.attr, contents = None)(
          elem.source, implicitly[NodeFactory.Aux[T]])
      case LiftableP(child) =>
        child
      case LiftableSpanStyle(attrs, body) =>
        logger.debug("[parent-child] liftable span")
        val meta = elem.attr.filter(_.key != STYLE) ++
          List(Attribute(STYLE, attrs.mkString(" ")))
        Repr.makeElem(elem.tag, body, attrs = meta, contents = None)(???, ???)
      case _ =>
        elem
    })
  }
}
