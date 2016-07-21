package net.lshift.typesetr.postprocessors

import net.lshift.typesetr.xml.Tag

import scala.annotation.tailrec
import scala.xml.{Elem, MetaData, Node, Text, UnprefixedAttribute}

trait PostProcessor {

  type ElemSig = (Tag, MetaData)

  def process(body: Any): Option[Any]

  protected def coalesce(nodes: List[Node]): List[Node]
}

trait PostProcessorUtils {
  self: PostProcessor =>

  sealed abstract class GroupKey
  case class TextKey(text: Text) extends GroupKey
  case class SigKey(elemSig: ElemSig) extends GroupKey

  // group together potential elements
  protected def coalesce(elems: List[Node]): List[Node] = {

    object BogusElement {
      def isBogus(x: Node): Boolean = unapply(x).isEmpty

      def unapply(x: Node): Option[Elem] = x match {
        case e: Elem =>
          e match {
            // empty void element
            case _ if e hasTag FULLY_VOID_TAGS =>
              Some(e)
            // emptyblock element
            case _ if (e hasTag NON_EMPTY_BLOCK_TAGS) && e.child.isEmpty =>
              Some(e)
            // empty link
            case _ if (e hasTag A) && (e hasAttribute NAME) =>
              Some(e)
            case _ =>
              None
          }
        case _ =>
          None
      }
    }

    def groupByTagAndAttr(x: Node): GroupKey =
      x match {
        // todo: other atoms?
        case text: Text =>
          TextKey(text)
        case elem: Elem =>
          SigKey((elem.label, elem.attributes))
      }

    def maybeCollapseGroups(key: ElemSig, elems: Seq[Node]): Seq[Node] =
      elems match {
        case singleElem :: Nil => elems
        case _ =>
          key._1 match {
            case inline if inline isIn INLINE_TAG_WITH_BLOCKQUOTE =>
              // weird odt splitting
              coalesceSiblings(key, elems).filterNot(BogusElement.isBogus)
            case BLOCK =>
              // collapse blocks into one
              coalesceBlocks(key, elems).filterNot(BogusElement.isBogus)
            // FIXME: missing LIT and CMD
            // checking
            case _ =>
              (for {
                elem <- elems
                elem1 <- coalesceParentChild(key, elem)
                elem2 <- coalesceHeadings(elem1)
              } yield elem2).flatten

          }
      }

    val grouped = elems.groupBy(groupByTagAndAttr _)

    (for {
      key <- grouped.keys.toList
      elems <- grouped(key)
    } yield {
      key match {
        case TextKey(_)  => elems
        case SigKey(key) => maybeCollapseGroups(key, elems)
      }
    }).flatten
  }

  // .block -> pre | blockquote handling
  // hacky and limited ATM; no support for nesting etc.
  private def coalesceBlocks(sig: ElemSig, elems: Seq[Node]): Seq[Node] = {

    @tailrec
    def codeBlock(elems: Seq[Node], txt: List[Option[Text]]): (Option[Node], Seq[Node]) = elems match {
      case (elem: Elem) :: rest if elem hasTag CODE =>
        // extract text from the code element
        codeBlock(rest, extractPlainText(elem) :: txt)
      case _ if txt.nonEmpty =>
        (Some(makeElem(PRE, Text(txt.flatten.mkString("\n")))), elems)
      case _ =>
        (None, elems)
    }

    @tailrec
    def nonCodeBlock(elems: Seq[Node], blockq: Seq[Node]): (Seq[Node], Seq[Node]) = elems match {
      case (elem: Elem) :: rest if elem hasTag CODE =>
        (optMakeElem(BLOCKQUOTE, blockq).getOrElse(Seq()), elems)

      case (elem: Elem) :: rest =>
        if (elem hasAttrWithVal ("class", "right")) {
          // append footer
          nonCodeBlock(rest,
            makeElem(FOOTER, Seq(makeElem(CITE, Seq(elem)))))
        } else {
          val toAppend =
            if ((elem hasTag BLOCK_TAGS) || (elem hasTag FOOTNOTE))
              // wrap in P
              makeElem(P, Seq(elem))
            else
              elem
          nonCodeBlock(rest, toAppend +: blockq)
        }

      case _ =>
        // No more blocks
        (optMakeElem(BLOCKQUOTE, blockq).getOrElse(Seq()), elems)
    }

    @tailrec
    def coalesceBlocks0(elems: Seq[Node], acc: Seq[Node]): Seq[Node] = elems match {
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

  private def coalesceSiblings(sig: ElemSig, elems: Seq[Node]): Seq[Node] = {
    // pack together the elements of the group
    // should apply cleaning up recursively
    val compactedElems = elems.flatMap(_.child)
    if (sig._1 == SPAN) compactedElems
    else makeElem(sig._1, compactedElems, Some(sig._2))
  }

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
  private def coalesceParentChild(sig: ElemSig, elem: Node): Option[Node] = {

    object BodyWithBogusP {
      val liftableTags = List(LI, DT, DD, FOOTNOTE)

      object DoesNotStartWithP {
        def unapply(elems: List[Node]): Option[Seq[Node]] = {
          if (elems exists (_.hasTag(P))) None
          else Some(elems)
        }
      }

      def unapply(elem: Node): Option[Seq[Node]] = {
        if (elem hasTag liftableTags)
          elem.child match {
            case (elem: Elem) :: DoesNotStartWithP(elems) if elem.hasTag(P) =>
              Some(elem.child ++ elems)
            case _ =>
              None
          }
        else None
      }
    }

    object LiftableP {
      val allowedInnerTags = List(PAGEBREAK, BLOCKQUOTE)

      def unapply(elem: Node): Option[Node] = elem match {
        case (elem: Elem) if elem.hasTag(P) &&
          elem.child.forall(_ hasTag allowedInnerTags) =>
          Some(elem)
        case _ =>
          None
      }
    }

    object LiftableSpanStyle {
      val invalidAttributes = List(COLOR, BACKGROUND_COLOR)

      def unapply(elem: Node): Option[(Seq[Node], Seq[Node])] = elem match {
        case (elem: Elem) if (elem hasTag SPAN) && (elem hasAttribute STYLE) =>
          val attrs: MetaData = elem.attributes.filter { metaData =>
            invalidAttributes.contains(xml.Attribute(metaData.key))
          }
          Some((attrs.flatMap(_.value).toSeq, elem.child))
        case _ =>
          None
      }
    }

    Some(elem match {
      case BodyWithBogusP(children) =>
        makeElem(elem.label, children, Some(elem.attributes))
      case LiftableP(_) =>
        elem
      case LiftableSpanStyle(attrs, body) =>
        val meta0 = elem.attributes.remove(STYLE)
        val meta = new UnprefixedAttribute(STYLE, attrs, meta0)
        makeElem(elem.label, body, Some(meta))
      case _ =>
        elem
    })
  }

  // Is there any actual textual content in the heading?
  private def coalesceHeadings(body: Node): Option[Seq[Node]] = {

    object IsAnchor {
      def unapply(node: Node): Option[Elem] = node match {
        case (elem: Elem) if (elem hasTag A) && (elem hasAttribute NAME) =>
          Some(elem)
        case _ =>
          None
      }
    }

    def isAnchor(node: Node): Boolean = IsAnchor.unapply(node).nonEmpty
    def isString(node: Node): Boolean = ???

    val (cleanedBody, noImgFig) =
      (for {
        bodyElem <- whack(body, n => !(n hasTag CAN_OCCUR_IN_HEADER))
        noImgElem <- whack(bodyElem, n => n hasTag List(IMG, FIGURE))
      } yield (bodyElem, noImgElem)).unzip

    if (isBlank(noImgFig)) {
      for {
        elem0 <- extractElem(body)
      } yield {
        elem0.copy(child = cleanedBody,
          attributes = elem0.attributes.remove("style"))
      }
    } else {
      val nodes = (for {
        elem <- cleanedBody if !isAnchor(elem) && !isString(elem)
      } yield elem)

      if (nodes.isEmpty) None else Some(nodes)
    }
  }

  def extractElem(node: Node): Option[Elem] = node match {
    case elem: Elem => Some(elem)
    case _          => None
  }

  private def extractPlainText(node: Node): Option[Text] = node match {
    case atom: Text =>
      Some(atom)
    case _ =>
      None
  }

}