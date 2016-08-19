package net.lshift.typesetr
package postprocessors

import net.lshift.typesetr.xml.Tag

import scala.annotation.tailrec
import scala.xml.{ Elem, MetaData, Node, Text, UnprefixedAttribute }
import xml._
import xml.InternalTags._
import xml.Attributes._
import net.lshift.typesetr.parsers.{ NodeFactory, TextRepr, Repr }

import util.Logger

abstract class PostProcessor {

  type ElemSig = (Tag, List[Attribute])

  def process[T](node: Repr.Aux[T])(implicit factory: NodeFactory[T], logger: Logger): Repr.Aux[T] = {
    val nodes = coalesce(node.body.toList)
    factory.create(
      tag = node.tag,
      elem = node.source,
      children = nodes,
      contents = node.contents,
      attrs = node.attr)
  }

  protected def coalesce[T](nodes: List[Repr.Aux[T]])(implicit factory: NodeFactory[T], logger: Logger): List[Repr.Aux[T]]

}

trait PostProcessorUtils extends OpimizerStrategies {
  self: PostProcessor =>

  import xml.TagGroups._

  // group together potential elements
  protected def coalesce[T](nodes: List[Repr.Aux[T]])(implicit factory: NodeFactory[T], logger: Logger): List[Repr.Aux[T]] = {

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

    def groupByTagAndAttr[T](x: Repr.Aux[T]): Int => GroupKey = (idx: Int) =>
      x.tag match {
        case Tag.textTag => TextKey(idx)
        case t =>
          t.state match {
            case Tag.Optimize => SigKey((t, x.attr), idx)
            case Tag.Leave    => SkolemKey(idx)
            case Tag.Replace  => RemoveKey(idx)
          }
      }

    def maybeCollapseGroups[T](key: ElemSig, elems: Seq[Repr.Aux[T]])(implicit builder: NodeFactory[T], logger: Logger): Seq[Repr.Aux[T]] = {
      elems match {
        case singleElem :: Nil => elems
        case _ =>
          logger.info(s"Collapse groups of ${key._1}")
          key._1 match {
            case inline if inline isIn INLINE_TAG_WITH_BLOCKQUOTE =>
              // weird odt splitting
              // TODO: enable filtering once styles are back
              coalesceSiblings(key, elems) //.filterNot(BogusElement.isBogus)
            case BLOCK =>
              // TODO: enable filtering once styles are back
              // collapse blocks into one
              coalesceBlocks(key, elems) //.filterNot(BogusElement.isBogus)
            // FIXME: missing LIT and CMD
            // checking
            case _ =>
              logger.info("Coalesce based on parent-child relation")
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

    val nodes1 = nodes.map(process(_)(factory, logger))

    // Group all the child nodes by the tag.
    // Note: when grouping we have to consider if they are not
    //       separated by some different group.
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
          Repr.makeTextElem(text)(factory.textNode(text), factory) :: Nil
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

trait OptimizerCoalesceBlocks {
  self: PostProcessor =>

  import xml.TagGroups._

  // .block -> pre | blockquote handling
  // hacky and limited ATM; no support for nesting etc.
  protected def coalesceBlocks[T](sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit factory: NodeFactory[T], logger: Logger): Seq[Repr.Aux[T]] = {

    @tailrec
    def codeBlock[T](elems: Seq[Repr.Aux[T]], txt: List[Option[String]]): (Option[Repr.Aux[T]], Seq[Repr.Aux[T]]) = elems match {
      case (elem: Repr) :: rest if elem hasTag CODE =>
        // extract text from the code element
        codeBlock(rest, elem.extractPlainText :: txt)
      case _ if txt.nonEmpty =>
        // TODO: collapse blocks into one xml node
        // that represents the ODT equivalent
        (Some(Repr.makeElem(PRE, txt.flatten.mkString("\n"))(???, ???)), elems)
      case _ =>
        (None, elems)
    }

    @tailrec
    def nonCodeBlock[T](elems: Seq[Repr.Aux[T]], blockq: Seq[Repr.Aux[T]])(implicit builder: NodeFactory[T]): (Seq[Repr.Aux[T]], Seq[Repr.Aux[T]]) = elems match {
      case (elem: Repr) :: rest if elem hasTag CODE =>
        (Repr.optMakeElem(BLOCKQUOTE, blockq).getOrElse(Seq()), elems)

      case (elem: Repr) :: rest =>
        if (elem hasAttrWithVal ("class", "right")) {
          // append footer
          val citation = Seq(Repr.makeElem(CITE, Seq(elem))(???, ???))
          nonCodeBlock[T](rest,
            Seq(Repr.makeElem(FOOTER, citation)(???, ???)))
        } else {
          val toAppend =
            if ((elem hasTag BLOCK_TAGS) || (elem hasTag FOOTNOTE))
              // wrap in P
              Repr.makeElem(P, Seq(elem))(???, ???)
            else
              elem
          nonCodeBlock(rest, toAppend +: blockq)
        }

      case _ =>
        // No more blocks
        (Repr.optMakeElem(BLOCKQUOTE, blockq).getOrElse(Seq()), elems)
    }

    @tailrec
    def coalesceBlocks0[T](elems: Seq[Repr.Aux[T]], acc: Seq[Repr.Aux[T]])(implicit builder: NodeFactory[T]): Seq[Repr.Aux[T]] = elems match {
      case Nil =>
        acc
      case _ =>
        val (codeNode0, rest1) = codeBlock(elems, Nil)
        val codeNode = codeNode0.map(n => Seq(n)).getOrElse(Seq())
        val (nonCodeNodes, rest2) = nonCodeBlock(rest1, Nil)
        coalesceBlocks0(rest2, codeNode ++ acc)
    }

    coalesceBlocks0(elems, Seq())
  }

}

trait OptimizerCoalesceSiblings {
  self: PostProcessor =>

  protected def coalesceSiblings[T](sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit factory: NodeFactory[T], logger: Logger): Seq[Repr.Aux[T]] = {
    // pack together the elements of the group
    // should apply cleaning up recursively
    val compactedElems = coalesce(elems.flatMap(_.body).toList)
    logger.info(s"coalesce siblings: $sig > Reduced ${elems.length} to ${compactedElems.length}")
    val r = elems match {
      case Nil => Nil
      case first :: _ =>
        if (sig._1 == SPAN) compactedElems
        // TODO, create a new XML node
        else Repr.makeElem(sig._1, compactedElems, sig._2)(first.source, factory) :: Nil
    }
    r
  }
}

trait OptimzerCoalesceHeadings {
  self: PostProcessor =>

  import xml.TagGroups._

  // Is there any actual textual content in the heading?
  protected def coalesceHeadings[T](body: Repr.Aux[T])(implicit builder: NodeFactory[T], logger: Logger): Option[Seq[Repr.Aux[T]]] = {

    object IsAnchor {
      def unapply(node: Repr): Option[Repr] = node match {
        case (elem: Repr) if (elem hasTag A) && (elem hasAttribute NAME) =>
          Some(elem)
        case _ =>
          None
      }
    }

    def isAnchor(node: Repr): Boolean =
      IsAnchor.unapply(node).nonEmpty

    def isString(node: Repr): Boolean = node.source match {
      case _: Text => true
      case _       => false
    }

    val (cleanedBody, noImgFig) =
      (for {
        bodyElem <- whack(body, n => !(n hasTag CAN_OCCUR_IN_HEADER))
        noImgElem <- whack(bodyElem, n => n hasTag List(IMG, FIGURE))
      } yield (bodyElem, noImgElem)).unzip

    if (isBlank(noImgFig)) {
      logger.debug(s"[optimizer] empty img figure $noImgFig in $body")
      Some(Seq(Repr.makeElem(body.tag, cleanedBody,
        body.attr.filter(_.key != STYLE))(body.source, implicitly[NodeFactory[T]])))
    } else {
      val nodes = (for {
        elem <- cleanedBody if !isAnchor(elem) && !isString(elem)
      } yield elem)

      if (nodes.isEmpty) {
        logger.debug(s"[optimizer] headings: non-anchor and non-string in $cleanedBody")
        None
      } else Some(nodes)
      Some(body :: Nil)
    }
  }
}

trait OpimizerStrategies {
  self: PostProcessor =>

  protected def coalesceBlocks[T](sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit factory: NodeFactory[T], logger: Logger): Seq[Repr.Aux[T]]
  protected def coalesceSiblings[T](sig: ElemSig, elems: Seq[Repr.Aux[T]])(implicit factory: NodeFactory[T], logger: Logger): Seq[Repr.Aux[T]]
  protected def coalesceHeadings[T](body: Repr.Aux[T])(implicit factory: NodeFactory[T], logger: Logger): Option[Seq[Repr.Aux[T]]]
  protected def coalesceParentChild[T](sig: ElemSig, elem: Repr.Aux[T])(implicit factory: NodeFactory[T], logger: Logger): Option[Repr.Aux[T]]
}

trait OpimizerCoalesceParentChild {
  self: PostProcessor =>

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
  protected def coalesceParentChild[T](sig: ElemSig, elem: Repr.Aux[T])(implicit nodeRepr: NodeFactory[T], logger: Logger): Option[Repr.Aux[T]] = {

    object BodyWithBogusP {
      val liftableTags = List(LI, DT, DD, FOOTNOTE)

      object DoesNotStartWithP {
        def unapply[T](elems: List[Repr.Aux[T]]) = {
          if (elems exists (_.hasTag(P))) None
          else Some(elems)
        }
      }

      def unapply[T](elem: Repr.Aux[T]) = {
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
      val allowedInnerTags = List(PAGEBREAK, BLOCKQUOTE)

      def unapply[T](elem: Repr.Aux[T]) = elem match {
        case (elem: Repr) if elem.hasTag(P) &&
          elem.body.forall(_ hasTag allowedInnerTags) =>
          Some(elem)
        case _ =>
          None
      }
    }

    object LiftableSpanStyle {
      val invalidAttributes = List(COLOR, BACKGROUND_COLOR)

      def unapply[T](elem: Repr.Aux[T]): Option[(Seq[Attribute], Seq[Repr.Aux[T]])] =
        elem match {
          case (elem: Repr) if (elem hasTag SPAN) && (elem hasAttribute STYLE) =>
            val attrs = elem.attr.filter { _.key in invalidAttributes }
            Some((attrs, elem.body))
          case _ =>
            None
        }
    }

    Some(elem match {
      case BodyWithBogusP(children) =>
        logger.debug("[parent-child] body with bogus p")
        Repr.makeElem(elem.tag, children, elem.attr)(???, ???)
      case LiftableP(_) =>
        elem
      case LiftableSpanStyle(attrs, body) =>
        logger.debug("[parent-child] liftable span")
        val meta = elem.attr.filter(_.key != STYLE) ++
          List(Attribute(STYLE, attrs.mkString(" ")))
        //val meta = new UnprefixedAttribute(STYLE, attrs, meta0)
        Repr.makeElem(elem.tag, body, meta)(???, ???)
      case _ =>
        elem
    })
  }
}

object Optimizer {
  private final lazy val opt: PostProcessor = new Opt

  private class Opt extends PostProcessor
    with PostProcessorUtils
    with OpimizerCoalesceParentChild
    with OptimizerCoalesceBlocks
    with OptimizerCoalesceSiblings
    with OptimzerCoalesceHeadings

  def apply(): PostProcessor = opt
}
