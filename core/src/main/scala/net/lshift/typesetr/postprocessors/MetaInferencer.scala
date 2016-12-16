package net.lshift.typesetr
package postprocessors

import net.lshift.typesetr.parsers.styles._
import net.lshift.typesetr.xml.InternalTags
import net.lshift.typesetr.parsers.{ NodeConfigs, NodeInfo, Repr }
import styles.MetaFromDocument

import scalaz.Tags._
import scalaz.Scalaz._

trait MetaInferencer[T] { self =>

  implicit protected def nodeConfig: NodeConfigs.WithNode[T]

  def inferMeta(root: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): Either[String, (Repr.Aux[T], MetaFromDocument)] = {
    // 1. Go through all the nodes
    val (body1, metaEntries) = (root.body.map { node =>
      if (node.tag == InternalTags.BODY) {
        inferMetaFromBody(node)
      } else (node, Nil)
    }).unzip

    // The order of occurrence matters.
    // 2. Combine different commands into a single meta dictionary
    val metaEntries1 = metaEntries.flatten
    val meta =
      metaEntries1.foldLeft(nodeConfig.metaExtractor) {
        case (meta, entry) => entry.includeIn(meta)
      }

    // Replace the original root with the given children nodes.
    val noChange = (root.body zip body1) forall { case (n1, n2) => n1 eq n2 }
    val root1 = if (noChange) root else root.copy(body1)
    Right((root1, meta))
  }

  /**
   * Does the paragraph have the important meta-document information?
   * Those are represented at the top level of the file by the underlined headers.
   * Note: this is roughly represented as
   * <p>
   *  <span><u>Some meta tag</u></span>
   *  Value associated with the meta tag
   * </p>
   */
  private object MetaParagraph {

    def unapply(elem: Repr.Aux[T]): Option[InferredMetaEntryNode] = elem.tag match {
      case InternalTags.P =>
        elem.body match {
          case SpanWithValue(cmd) if cmd.key != "caption" =>
            Some(cmd)
          case others =>
            None
        }
      case _ => None
    }

    object MetaSpan {
      def unapply(elem: Repr.Aux[T]): Option[Repr.Aux[T]] = elem.tag match {
        case InternalTags.U => elem.body.headOption
        case _              => None
      }
    }

    object SpanWithValue {
      def unapply(elems: Seq[Repr.Aux[T]]): Option[InferredMetaEntryNode] = {
        elems match {
          case MetaSpan(cmd) :: rest =>
            RegularMetaEntryNode(cmd, rest)
          case _ =>
            None
        }
      }
    }

  }

  /**
   * Some command nodes are identified through the style
   * that is being applied to the node.
   */
  private object MetaHeader {

    private val allowedStyleTypes = (sTpe: StyleType) => sTpe.isInstanceOf[TitleStyleType]

    private def inferStyleKind(style: Style)(implicit docStyle: DocumentStyle.Aux[T]): Option[StyleType] =
      scalaz.Tag.unwrap(
        First(style.tpe.filter(allowedStyleTypes)) |+|
          First(style.parent.flatMap(docStyle.style).flatMap(_.tpe).filter(allowedStyleTypes)))

    def unapply(elem: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): Option[InferredMetaEntryNode] = {
      val style = docStyle.styleForNode(elem)(self.nodeConfig.styleExtractor, self.nodeConfig.nodeInfo)
      style.flatMap(inferStyleKind).flatMap(kind => StyleMetaEntryNode(kind, elem))
    }

  }

  /**
   * Looks at the top-level nodes of the document,
   * and infers from them meta entries, if possible.
   *
   * Note: the inference of a meta entry means that the corresponding
   * node is removed from the document.
   *
   * @param bodyNode node representing the body of the document
   * @param docStyle existing style dictionary
   * @return
   */
  private def inferMetaFromBody(bodyNode: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): (Repr.Aux[T], List[InferredMetaEntryNode]) = {

    /**
     * Takes a document's node and classifies it as
     * a) either containing a style information
     * b) or no information, and leaves the node as is
     */
    def classifyElement(elem: Repr.Aux[T]): Either[InferredMetaEntryNode, Repr.Aux[T]] = elem match {
      case MetaParagraph(cmd) =>
        Left(cmd)
      case MetaHeader(cmd) =>
        Left(cmd)
      case _ =>
        Right(elem)
    }

    /**
     * Look at a single node, and it's nesting within the body
     * of the document.
     *
     * @return a node, with possibly updated child nodes, and the inferred entries
     */
    def analyzeBodyNodes(elem: Repr.Aux[T], nesting: Int): (Repr.Aux[T], List[InferredMetaEntryNode]) = {
      if (nodeConfig.nodeInfo.isContentInBody(elem, nesting)) {
        val nodesOrCmd = elem.body map classifyElement
        val (nodes, cmds) = nodesOrCmd.foldRight((Nil: List[Repr.Aux[T]], Nil: List[InferredMetaEntryNode])) {
          case (Left(cmd), acc)   => (acc._1, cmd :: acc._2)
          case (Right(node), acc) => (node :: acc._1, acc._2)
        }
        (elem.copy(nodes), cmds)
      } else if (nodeConfig.nodeInfo.canContainContent(elem, nesting)) {
        val nodesAndCmds = elem.body.map(analyzeBodyNodes(_, nesting + 1)).unzip
        (elem.copy(nodesAndCmds._1), nodesAndCmds._2.flatten.toList)
      } else (elem, Nil)
    }

    val nodesAndCmds = bodyNode.body.map(analyzeBodyNodes(_, 0)).unzip
    (bodyNode.copy(nodesAndCmds._1), nodesAndCmds._2.flatten.toList)

  }

  /**
   *
   * Class for representing a single piece of style information
   * inferred from the document
   */
  sealed abstract class InferredMetaEntryNode {

    /**
     * Meta-key inferred from the document
     */
    def key: String

    /**
     * Meta-value inferred from the document
     */
    def value: String

    /**
     * Include that style information in the meta data
     *
     * @param meta initial meta data
     * @return meta data updated with this style
     */
    def includeIn(meta: MetaFromDocument): MetaFromDocument

  }

  private case class StyleMetaEntryNode(key: String, value: String)(styleInfo: StyleType) extends InferredMetaEntryNode {

    def includeIn(meta: MetaFromDocument): MetaFromDocument = styleInfo match {
      case TitleStyleTpe =>
        meta.withTitle(value)
      case SubTitleStyleTpe =>
        meta.withSubTitle(value)
      case _ =>
        meta
    }

  }

  private object StyleMetaEntryNode {
    def apply(styleInfo: StyleType, valueNode: Repr.Aux[T]): Option[InferredMetaEntryNode] =
      for {
        key <- withTitleKind(styleInfo)
        v <- valueNode.extractPlainText(deep = true)
      } yield StyleMetaEntryNode(key, v)(styleInfo)

    private def withTitleKind(styleInfo: StyleType) = styleInfo match {
      case tpe: TitleStyleType => Some(tpe.name)
      case _                   => None
    }
  }

  private case class RegularMetaEntryNode(key: String, value: String) extends InferredMetaEntryNode {

    def includeIn(meta: MetaFromDocument): MetaFromDocument =
      meta.withKey(key, value)

  }

  private object RegularMetaEntryNode {

    private def nonEmptyList[T](xs: List[T]): Option[List[T]] = xs match {
      case Nil => None
      case xs  => Some(xs)
    }

    def apply(cmdNode: Repr.Aux[T], valueNodes: List[Repr.Aux[T]]): Option[InferredMetaEntryNode] = {
      for {
        txt <- cmdNode.extractPlainText(deep = true) if txt.endsWith(":")
        values <- nonEmptyList(valueNodes.flatMap(_.extractPlainText(deep = true)))
      } yield RegularMetaEntryNode(txt.stripSuffix(":").toLowerCase, values.mkString("").trim)
    }
  }

}
