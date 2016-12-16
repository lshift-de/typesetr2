package net.lshift.typesetr
package postprocessors

import net.lshift.typesetr.pandoc.{ UUIDGen, Markers }
import parsers.styles.{ DocumentStyle, StyleId }
import net.lshift.typesetr.xml.attributes.TextAlign

import xml._
import xml.InternalTags._
import xml.Attributes._
import parsers._

import util.Logger

import scala.annotation.tailrec

import scalaz.Tags.First
import scalaz.Scalaz._

trait Optimizer[T] {

  type ElemSig = (Tag, List[Attribute], Option[StyleId])

  def optimize(node: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Repr.Aux[T] = {
    logger.info("Optimizing document")
    val nodes = coalesce(node.body.toList)
    implicitly[NodeFactory.Aux[T]].create(
      tag = node.tag,
      docNode = node.source,
      children = nodes,
      contents = node.contents,
      attrs = node.attr)
  }

  /**
   * Combine the nodes of the document, if possible.
   *
   * @param nodes a list of initial nodes to optimize
   * @param logger Typesetr's internal logging instance
   * @param sty document's stylesheet
   * @return a list of nodes, potentially combined/modified
   */
  protected def coalesce(nodes: List[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): List[Repr.Aux[T]]

  implicit protected def nodeConfig: NodeConfigs.WithNode[T]

  /**
   * A token for generating Typesetr's tags that are unique for
   * the given compilation phase
   *
   * @return a unique token
   */
  implicit protected def uuid: UUIDGen

}

trait OpimizerStrategies[T] {
  self: Optimizer[T] =>

  protected def coalesceBlocks(sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Seq[Repr.Aux[T]]

  protected def coalesceSiblings(sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Seq[Repr.Aux[T]]

  protected def coalesceHeadings(body: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Option[Seq[Repr.Aux[T]]]

  protected def coalesceParentChild(sig: ElemSig, elem: Repr.Aux[T])(implicit logger: Logger, sty: DocumentStyle.Aux[T]): Option[Repr.Aux[T]]

  protected def coalesceParagraphs(elems: List[Repr.Aux[T]])(implicit logger: Logger, prev: Option[Tag]): List[Repr.Aux[T]]

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
            case Tag.Optimize if nodeConfig.nodeInfo.isFormattedText(x) =>
              SigKey((t, x.attr, nodeConfig.styleExtractor.extractId(x)), idx)
            case Tag.Optimize => SigKey((t, x.attr, None), idx)
            case Tag.Leave    => SkolemKey(idx)
            case Tag.Replace  => RemoveKey(idx)
          }
      }

    def maybeCollapseGroups(key: ElemSig, elems: Seq[Repr.Aux[T]], prev: Option[(Tag, Seq[Repr.Aux[T]])])(implicit logger: Logger): Seq[Repr.Aux[T]] = {
      logger.debug(s"Collapse groups of ${key._1}")

      key._1 match {
        case inline if inline isIn INLINE_TAG_WITH_BLOCKQUOTE =>
          // weird odt splitting
          // TODO: enable filtering once styles are back
          logger.debug(s"[optimizer] coalesce siblings of ${elems.length} elements")
          coalesceSiblings(key, elems) //.filterNot(BogusElement.isBogus)
        case BLOCK =>
          // collapse blocks into one
          logger.debug("[optimizer] coalesce blocks")
          coalesceBlocks(key, elems) //.filterNot(BogusElement.isBogus)
        case P =>
          logger.debug("[optimizer] coalesce paragraph")
          coalesceParagraphs(elems.toList)(logger, prev.map(_._1))
        case _ =>
          logger.debug(s"[optimizer] coalesce based on parent-child relation ${key._1}")

          (for {
            elem <- elems
            elem1 <- coalesceParentChild(key, elem)
            elem2 <- coalesceHeadings(elem1)
          } yield elem2).flatten
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
    val groupedKeys = grouped.keys.toList.sortBy(_.idx)

    def findTag(idx: Int, from: List[GroupKey]): Option[(Tag, Seq[Repr.Aux[T]])] =
      from.find(_.idx == idx).flatMap {
        case k @ SigKey(key, _) => grouped.get(k).map(elems => (key._1, elems.map(_._1)))
        case _                  => None
      }

    (for {
      key <- groupedKeys
      elems0 <- grouped.get(key)
    } yield {
      val elems = elems0.map(_._1)
      key match {
        case TextKey(_) =>
          val text = elems.flatMap(_.extractPlainText).mkString("")
          Repr.makeTextElem(text) :: Nil
        case SigKey(key, idx) =>
          logger.debug(s"[optimizer] maybe collapse a node with $key and ${elems.headOption.map(_.source)}")
          maybeCollapseGroups(key, elems, findTag(idx - 1, groupedKeys))
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
  case class SigKey(elemSig: ElemSig, idx: Int) extends GroupKey {
    override def equals(x: Any): Boolean =
      x match {
        case SigKey(elemSig2, idx2) =>
          (elemSig2._1 equals elemSig._1) &&
            (elemSig2._2 equals elemSig._2) &&
            (elemSig2._3 equals elemSig._3) &&
            (idx2 == idx)
        case _ =>
          false
      }
  }
  case class SkolemKey(idx: Int) extends GroupKey
  case class RemoveKey(idx: Int) extends GroupKey

}

trait OptimizerCoalesceParagraphs[T] {
  self: Optimizer[T] =>

  def coalesceParagraphs(elems: List[Repr.Aux[T]])(implicit logger: Logger, prev: Option[Tag]): List[Repr.Aux[T]] =
    paragraphSlidingWindow(elems, acc = Nil)

  // Figure and its caption in separate paragraphs but still next to each other
  private def paragraphSlidingWindow(elems: List[Repr.Aux[T]], acc: List[Repr.Aux[T]])(implicit logger: Logger, prev: Option[Tag]): List[Repr.Aux[T]] =
    elems match {
      case (p @ FigureP(pre, figElem, post)) :: (c @ CaptionP(txtElems)) :: rest =>
        // create a new figure with a caption
        val postTxt = post.flatMap(_.extractPlainText(deep = true)).mkString("")
        if (postTxt.trim != "") {
          logger.info("A text between a figure and a caption is non-empty. Ignoring caption.")
          paragraphSlidingWindow(p :: rest, acc)
        } else {
          val withInferredCaption = implicitly[NodeFactory.Aux[T]].imgWithCaption(figElem, txtElems)
          val newP = implicitly[NodeFactory.Aux[T]].paragraphFrom(
            pre.flatMap(parseImages) ++ Seq(withInferredCaption), p)
          paragraphSlidingWindow(rest, newP :: acc)
        }

      // Figure and caption in the same paragraph
      case (p @ FigureWithCaptionP(pre, figElem, caption)) :: rest =>
        val withInferredCaption = implicitly[NodeFactory.Aux[T]].imgWithCaption(figElem, caption)
        val newP = implicitly[NodeFactory.Aux[T]].paragraphFrom(
          pre.flatMap(parseImages) ++ Seq(withInferredCaption), p)
        paragraphSlidingWindow(rest, newP :: acc)

      case (p @ FigureP(pre, fig, post)) :: rest =>
        val imgKindOpt = hasImgAttribute(fig.attr)
        val newP = imgKindOpt.map {
          case (imgKind0, width) =>
            val imgKind = if (post.isEmpty && pre.isEmpty) BlockImg else imgKind0
            val body1: Seq[Repr.Aux[T]] =
              pre.flatMap(parseImages) ++ imgKind.formatting.format(Seq(fig)) ++ post.flatMap(parseImages)
            implicitly[NodeFactory.Aux[T]].paragraphFrom(body1, p)
        } getOrElse (p)
        paragraphSlidingWindow(rest, newP :: acc)

      case (elem @ CaptionP(txtElems)) :: rest if prev == Some(TABLE) =>
        val attr1 = Attribute(InternalAttributes.style, "Table") :: Nil
        val tableCaption = implicitly[NodeFactory.Aux[T]].paragraphFrom(txtElems, elem)
        paragraphSlidingWindow(rest, tableCaption.copy(attr = attr1) :: acc)

      case (elem @ CaptionP(txtElems)) :: rest =>
        logger.info(s"""Ignoring caption ${txtElems.map(_.extractPlainText).mkString("")} not associated with any image/table""")
        paragraphSlidingWindow(rest, elem :: acc)

      case head :: rest =>
        paragraphSlidingWindow(rest, head :: acc)

      case _ =>
        acc.reverse
    }

  private def hasImgAttribute(attrs: List[Attribute]): Option[(ImageKind, Double)] =
    for {
      imgKindRaw <- InternalAttributes.frameDisplay inAttributes attrs flatMap (_.value)
      imgWidthRaw <- InternalAttributes.imgWidth inAttributes attrs flatMap (_.value)
      imgKind <- ImageKind(imgKindRaw)
    } yield (imgKind, imgWidthRaw.toDouble)

  private object FigureP {
    // Extracts the last frame element and whatever was pre- and post- of it, if any.
    def unapply(elem: Repr.Aux[T]): Option[(List[Repr.Aux[T]], Repr.Aux[T], List[Repr.Aux[T]])] = {
      // Only attach the caption to the last frame node
      val spanned = elem.body.foldRight((Nil: List[Repr.Aux[T]], Nil: List[Repr.Aux[T]])) {
        case (elem, acc) =>
          if (acc._1.nonEmpty) (elem :: acc._1, acc._2)
          else if (elem.tag == InternalTags.FRAME) (elem :: Nil, acc._2)
          else (Nil, elem :: acc._2)
      }
      spanned._1.reverse match {
        case head :: rest => Some((rest, head, spanned._2))
        case Nil          => None
      }
    }
  }

  private object FigureWithCaptionP {
    // Extracts the last frame element and whatever was pre- and post- of it, if any.
    def unapply(elem: Repr.Aux[T]): Option[(List[Repr.Aux[T]], Repr.Aux[T], List[Repr.Aux[T]])] = {
      // Only attach the caption to the last frame node
      val spanned = elem.body.foldRight((Nil: List[Repr.Aux[T]], Nil: List[Repr.Aux[T]])) {
        case (elem, acc) =>
          if (acc._1.nonEmpty) (elem :: acc._1, acc._2)
          else if (elem.tag == InternalTags.FRAME) (elem :: Nil, acc._2)
          else (Nil, elem :: acc._2)
      }
      (spanned._1.reverse, spanned._2) match {
        case (head :: rest, caption :: rest2) =>
          if (caption.tag == InternalTags.CAPTION)
            Some((rest, head, rest2))
          else
            None
        case _ => None
      }
    }
  }

  private object CaptionP {
    def unapply(elem: Repr.Aux[T]): Option[List[Repr.Aux[T]]] =
      elem.body.headOption.filter(_.tag == InternalTags.CAPTION).map(_ => elem.body.tail.toList)
  }

  private def parseImages(elem: Repr.Aux[T]): Seq[Repr.Aux[T]] =
    hasImgAttribute(elem.attr).map(kind => kind._1.formatting.format(Seq(elem))).getOrElse(Seq(elem))

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
      case (elem) :: rest if elem hasTag BLOCKCODE =>
        codeBlock(rest, elem +: codeElems)
      case _ =>
        (codeElems, elems)
    }

    @tailrec
    def extractNonCodeNodes(elems: Seq[Repr.Aux[T]], blockq: Seq[Repr.Aux[T]])(implicit blockParent: Repr.Aux[T]): (Seq[Repr.Aux[T]], Seq[Repr.Aux[T]]) = elems match {
      case (elem: Repr) :: rest if elem hasTag BLOCKCODE =>
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
      else {
        val code = codeElems0.reverse
        Some(
          Repr.makeElem(
            tag = PRE,
            body = Markers.formatBlock(code),
            contents = None,
            attrs = Nil)(paragraphBlock.source, implicitly[NodeFactory.Aux[T]]))
      }
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
    logger.debug(s"coalesce siblings: $sig")

    // pack together the elements of the group
    // should apply cleaning up recursively
    elems match {
      case StyleSpanWithinSpan(elem) :: Nil =>
        coalesce(elem :: Nil)
      case _ =>
        // do an actual coalescing, that is not related to text formatting
        val compactedElems = coalesce(elems.flatMap(_.body).toList)

        logger.debug(s"Reduced ${elems.length}")

        elems.headOption.map { first =>
          Repr.makeElem(sig._1, compactedElems, contents = None,
            attrs = sig._2)(first.source, implicitly[NodeFactory.Aux[T]]) :: Nil
        } getOrElse (compactedElems)
    }

  }

  object SpanElem {
    def unapply(elem: Repr.Aux[T]): Option[Repr.Aux[T]] =
      if (nodeConfig.nodeInfo.isFormattedText(elem)) Some(elem)
      else None
  }

  object StyleSpanWithinSpan {

    // Span elements with other Span elements with the same
    // style name is an unnecessary structure. Collapse them
    // into one.
    def unapply(elem: Repr.Aux[T]): Option[Repr.Aux[T]] = {
      elem match {
        case SpanElem(elem1) =>
          elem1.body match {
            case SpanElem(elem2) :: Nil =>
              val styleInner = nodeConfig.styleExtractor.extractId(elem2)
              val styleOuter = nodeConfig.styleExtractor.extractId(elem1)
              if (styleInner == styleOuter) Some(elem2)
              else None
            case _ => None
          }
        case _ => None
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

    // Pandoc can handle those paragraphs easily
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
            case (elem1: Repr) :: DoesNotStartWithP(elems) if elem1.hasTag(P) =>
              Some(elem1.body ++ elems)
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

    object SpanElem {
      def unapply(elem: Repr.Aux[T]): Option[Repr.Aux[T]] = elem.tag match {
        case SPAN => Some(elem)
        case _    => None
      }
    }

    object SpanWithinSpan {

      def unapply(elem: Repr.Aux[T]): Option[Repr.Aux[T]] = {
        elem match {
          case SpanElem(elem1) =>
            elem1.body match {
              case SpanElem(elem2) :: Nil => Some(elem2)
              case _                      => None
            }
          case _ => None
        }
      }

    }

    Some(elem match {
      /*case BodyWithBogusP(children) =>
        logger.debug("[parent-child] body with bogus p")
        Repr.makeElem(elem.tag, children, attrs = elem.attr, contents = None)(
          elem.source, implicitly[NodeFactory.Aux[T]])*/
      case LiftableP(child) =>
        child
      case SpanWithinSpan(insideElem) =>
        insideElem
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
