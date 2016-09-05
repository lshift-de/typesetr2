package net.lshift.typesetr
package postprocessors

import parsers.odt.styles._
import parsers.{ NodeInfo, DocumentStyle, Repr }
import styles.MetaFromDocument

import scalaz.Tags._
import scalaz.Scalaz._

trait MetaInferencer[T] {
  self: PostProcessor[T] =>

  def extractMeta(root: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): MetaFromDocument = {
    // Apparently additional meta information is only
    // extracted from the top nodes of the content.
    val body = implicitly[NodeInfo.Aux[T]].docContent(root)
    body.foldLeft(self.nodeConfig.metaExtractor) {
      case (meta0, node) if meta0.isUpdateable =>
        extractMetaFromNode(node, meta0)(docStyle)
      case (meta0, node) =>
        meta0
    }
  }

  private def extractMetaFromNode(node: Repr.Aux[T], meta: MetaFromDocument)(
    implicit docStyle: DocumentStyle.Aux[T]): MetaFromDocument = {
    val metaInfo = {
      for {
        style <- docStyle.styleForNode(node)(self.nodeConfig.styleExtractor)
        tpe <- hasMetaInfo(style)
      } yield updateMeta(node, tpe)(meta)
    }

    metaInfo.getOrElse(meta)
  }

  private def hasMetaInfo(style: Style)(implicit docStyle: DocumentStyle.Aux[T]): Option[StyleType] = {
    scalaz.Tag.unwrap(
      First(style.tpe.filter(isMetaStyle)) |+|
        First(style.parent.flatMap(docStyle.style).flatMap(_.tpe).filter(isMetaStyle)))
  }

  private def isMetaStyle(styleKind: StyleType): Boolean =
    styleKind match {
      case _: TitleStyleType => true
      case _                 => false
    }

  private def updateMeta(node: Repr.Aux[T], kind: StyleType)(implicit meta: MetaFromDocument): MetaFromDocument =
    kind match {
      case TitleStyleTpe =>
        node.extractPlainText(deep = true).map(meta.withTitle).getOrElse(meta)
      case SubTitleStyleTpe =>
        node.extractPlainText(deep = true).map(meta.withSubTitle).getOrElse(meta)
      case _ =>
        meta
    }
}
