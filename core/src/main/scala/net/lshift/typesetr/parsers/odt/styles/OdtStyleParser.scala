package net.lshift.typesetr
package parsers
package odt
package styles

import net.lshift.typesetr.parsers.styles._
import odt.OdtTags
import xml.{XmlTag, NameSpaces}
import xml.attributes._

import scala.xml.Node
import shapeless._
import syntax.singleton._
import record._


import scalaz.Tags.First
import scalaz.Scalaz._

import util.Logger

import scala.language.implicitConversions

class OdtStyleParser extends StyleParser {

  type DocNode = scala.xml.Node

  import OdtStyleParser._

  def loadFromDocContent(rootStyle: Node, doc: DocumentStyle.Aux[DocNode])(implicit logger: Logger): DocumentStyle.Aux[DocNode] = {
    (for {
      docWithStyles <- parseGroupNode(rootStyle \!! OdtTags.Styles, doc)
      docWithAutoStyles <- parseGroupNode(rootStyle \!! OdtTags.AutomaticStyle, docWithStyles)
    } yield docWithAutoStyles) getOrElse (doc)
  }

  def appendStyleNode(node: scala.xml.Node, doc: DocumentStyle.Aux[DocNode])(implicit logger: Logger): DocumentStyle.Aux[DocNode] = {
    (for {
      name <- node.attributes.getTag(OdtTags.StyleName)
      styleInfo <- parseStyle(node, doc)
      styleId <- OdtStyleId.fromNode(node)
    } yield (styleId, styleInfo) +: doc) getOrElse doc
  }

  private def parseGroupNode(node: Option[Node], doc: DocumentStyle.Aux[DocNode])(implicit logger: Logger): Option[DocumentStyle.Aux[DocNode]] =
    node match {
      case None       => Some(doc)
      case Some(node) =>
        Some((node.child).foldLeft(doc) { case (doc, node) =>
          (for {
            name      <- node.attributes.getTag(OdtTags.StyleName)
            styleInfo <- parseStyle(node, doc)
            styleId   <- OdtStyleId.fromNode(node)
          } yield (styleId, styleInfo) +: doc) getOrElse doc
        })
    }

  protected def parseStyle(styleNode: Node, doc: DocumentStyle)(implicit logger: Logger): Option[Style] = {
    if (invalidNodes.contains(styleNode.xmlTag)) {
      logger.info(s"Ignoring node $styleNode")
      None
    } else {
      val id = OdtStyleId.fromNode(styleNode)
      val parent = styleNode.attributes.getTag(OdtTags.StyleParentStyle)

      lazy val parentStyle = (for {
        styleId <- id
        pName <- parent
        pStyle <- doc.style(styleId.withThisFamily(pName))
      } yield pStyle)

      val tpe = styleNode.xmlTag match {
        case OdtTags.StyleStyle =>

          val styleTpe =
            // Is it header?
            First(styleNode.attributes.getTag(OdtTags.StyleDisplayName).flatMap(HeadingStyle.unapply)) |+|
            // Is parent a title?
            First(parent.flatMap(TitleStyleType.unapply)) |+|
            // Is current node a title?
            First(id.flatMap(s => TitleStyleType.unapply(s.name))) |+|
            // Inherit style type from the parent node if any
            First(parentStyle.flatMap(_.tpe)) |+|
            // If the family type of the style is a paragraph, then it's a paragraph
            First(id.filter(_.family == Some(ParagraphFamily)).map(_ => PStyleTpe)) |+|
            // If the family type of the style is table, them it's a table
            First(id.flatMap(s => s.family.flatMap(TableStyleType.unapply))) |+|
            // Otherwise it's just a span
            First(Some(SpanStyleTpe))

          scalaz.Tag.unwrap(styleTpe)
        case OdtTags.TextListStyle =>
          Some(ListStyleTpe)
      }

      /*
       * For each style property:
       *  - extract the appropriate xml node from text, paragraph and/or table xml nodes
       *  - validate it
       *  - transform the string value into a first class Scala object
       */
      class toMap(props: Props) extends Poly1 {
        // Creates a single (StyleProperty -> StylePropertyValue) record.
        implicit def conv[T <: StylePropKey](implicit x: T) =
          at[T] { _ =>
            (x ->>
              prop(x)(
                StylePropertyValidators.map(x)(StylePropertyValidators.map.caseRel),
                StylePropertyExtractors.map(x)(StylePropertyExtractors.map.caseRel).
                  build(props))) }


        private def prop[T <: StylePropKey](x: T)(styleValidator: StyleValidator.Aux[x.Result],
                                                  propExtractor: PropertiesExtractor): Option[x.Result] = {
          x.name flatMap { xmlTag =>
            val node = propExtractor.extract(xmlTag)
            if (styleValidator.validate(node)) node.flatMap(styleValidator.transform)
            else None
          }
        }

      }

      val mappingFun = new toMap(styleNode)
      import mappingFun._

      val styleMap = OdtStyle.textStyleProperties.map(mappingFun)
      id map (styleId =>
        OdtStyle.typesafeStyle(styleMap, styleId, parentStyle.map(_.id), tpe)(styleNode))
    }
  }

  private def parseListStyle(styleNode: Node): Option[Style] = ???

}

object OdtStyleParser {
  final val invalidNodes =
    Seq(
      OdtTags.StyleDefault,
      OdtTags.StylePageLayout,
      OdtTags.TextOutlineStyle,
      OdtTags.TextNotesConf,
      OdtTags.TextLineNumConf)

  lazy val defaultOdt: StyleParser.Aux[scala.xml.Node] = new OdtStyleParser()

  private final val ParagraphFamily = "paragraph"

}
