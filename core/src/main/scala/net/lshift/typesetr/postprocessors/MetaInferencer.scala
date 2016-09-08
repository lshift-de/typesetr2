package net.lshift.typesetr
package postprocessors

import net.lshift.typesetr.xml.InternalTags
import parsers.odt.styles._
import net.lshift.typesetr.parsers.{ NodeConfigs, NodeInfo, DocumentStyle, Repr }
import styles.MetaFromDocument

import scalaz.Tags._
import scalaz.Scalaz._

trait MetaInferencer[T] { self =>

  implicit protected def nodeConfig: NodeConfigs.WithNode[T]

  def inferMeta(root: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): (Repr.Aux[T], MetaFromDocument) = {
    // 1. Go through all the nodes
    val childrenAndCmds = (root.body.map { node =>
      if (node.tag == InternalTags.BODY) {
        inferFromBody(node)
      } else (node, Nil)
    }).unzip

    // The order of occurrence matters.
    // Combine different commands into a single meta-sheet
    val meta =
      childrenAndCmds._2.flatten.foldLeft(nodeConfig.metaExtractor) {
        case (meta, cmd) => cmd.includeIn(meta)
      }

    (root.copy(childrenAndCmds._1), meta)
  }

  /**
   * Does the paragraph has the important meta-document information?
   * Those are represented at the top level of the file by the underlined headers.
   */
  private object CommandParagraph {

    def unapply(elem: Repr.Aux[T]): Option[CommandNode] = elem.tag match {
      case InternalTags.P =>
        elem.body match {
          case CommandSpan(cmd) if cmd.cmd.nonEmpty && cmd.cmd.get != "caption" =>
            Some(cmd)
          case others =>
            None
        }
      case _ => None
    }

    object CommandSpan {
      def unapply(elems: Seq[Repr.Aux[T]]): Option[CommandNode] =
        elems match {
          case cmd :: rest if cmd.tag == InternalTags.U =>
            Some(new RegularCommandNode(cmd, rest))
          case _ =>
            None
        }
    }

  }

  /**
   * Some command nodes are identified through the style
   * that is being applied to the node.
   */
  private object StyleCommandHeader {

    def unapply(elem: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): Option[CommandNode] = {
      val style = docStyle.styleForNode(elem)(self.nodeConfig.styleExtractor)
      style.flatMap(hasMetaInfo).map(kind => new StyleCommandNode(kind, elem))
    }

  }

  private def inferFromBody(bodyNode: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): (Repr.Aux[T], List[CommandNode]) = {

    def elementsOfBody(elem: Repr.Aux[T]): Either[CommandNode, Repr.Aux[T]] = elem match {
      case CommandParagraph(cmd) =>
        Left(cmd)
      case StyleCommandHeader(cmd) =>
        Left(cmd)
      case _ =>
        Right(elem)
    }

    def analyzeBodyNodes(elem: Repr.Aux[T], nesting: Int): (Repr.Aux[T], List[CommandNode]) = {
      if (nodeConfig.nodeInfo.isContentInBody(elem, nesting)) {
        val nodesOrCmd = elem.body map elementsOfBody
        val (nodes, cmds) = nodesOrCmd.foldRight((Nil: List[Repr.Aux[T]], Nil: List[CommandNode])) {
          case (Left(cmd), acc)   => (acc._1, cmd :: acc._2)
          case (Right(node), acc) => (node :: acc._1, acc._2)
        }
        (elem.copy(nodes), cmds)
      } else if (nodeConfig.nodeInfo.canContainContent(elem, nesting)) {
        val nodesAndCmds = elem.body.map(analyzeBodyNodes(_, nesting + 1)).unzip
        (elem.copy(nodesAndCmds._1), nodesAndCmds._2.flatten.toList)
      } else (elem, Nil)
    }

    // Assumes the same of level nesting for every document, which is a mistake.
    val nodesAndCmds = bodyNode.body.map(analyzeBodyNodes(_, 0)).unzip
    // TODO: create a new node
    (bodyNode.copy(nodesAndCmds._1), nodesAndCmds._2.flatten.toList)

  }

  private def isMetaStyle(styleKind: StyleType): Boolean =
    styleKind match {
      case _: TitleStyleType => true
      case _                 => false
    }

  private def hasMetaInfo(style: Style)(implicit docStyle: DocumentStyle.Aux[T]): Option[StyleType] = {
    scalaz.Tag.unwrap(
      First(style.tpe.filter(isMetaStyle)) |+|
        First(style.parent.flatMap(docStyle.style).flatMap(_.tpe).filter(isMetaStyle)))
  }

  abstract class CommandNode {

    def cmd: Option[String]

    def value: Option[String]

    def includeIn(meta: MetaFromDocument): MetaFromDocument

  }

  private class StyleCommandNode(styleInfo: StyleType, valueNode: Repr.Aux[T]) extends CommandNode {

    def cmd: Option[String] = styleInfo match {
      case TitleStyleTpe    => Some("title")
      case SubTitleStyleTpe => Some("subtitle")
      case _                => None
    }

    def value: Option[String] = valueNode.extractPlainText(deep = true)

    def includeIn(meta: MetaFromDocument): MetaFromDocument = styleInfo match {
      case TitleStyleTpe =>
        value.map(meta.withTitle).getOrElse(meta)
      case SubTitleStyleTpe =>
        value.map(meta.withSubTitle).getOrElse(meta)
      case _ =>
        meta
    }

  }

  private class RegularCommandNode(cmdNode: Repr.Aux[T], valueNodes: List[Repr.Aux[T]]) extends CommandNode {

    def cmd: Option[String] =
      cmdNode.extractPlainText(deep = true).flatMap { txt =>
        if (txt.endsWith(":")) Some(txt.stripSuffix(":").toLowerCase)
        else None
      }

    def value: Option[String] =
      valueNodes.flatMap(_.extractPlainText(deep = true)) match {
        case Nil    => None
        case values => Some(values.mkString("").trim)
      }

    def isValid: Boolean = cmd.nonEmpty && value.nonEmpty

    def includeIn(meta: MetaFromDocument): MetaFromDocument =
      (for {
        key <- cmd
        v <- value
      } yield meta.withKey(key, v)).getOrElse(meta)

  }

}
