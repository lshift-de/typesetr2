package net.lshift.typesetr
package parsers
package odt
package styles

import net.lshift.typesetr.parsers.odt.styles.Style.MMap
import odt.{DocumentStyle, OdtTags}
import net.lshift.typesetr.xml
import net.lshift.typesetr.xml.{XmlTag, NameSpaces}
import xml.attributes._

import scala.xml.Node
import shapeless._
import syntax.singleton._
import record._

import util.Logger

import scalaz.Tags.First
import scalaz.Scalaz._

import scala.language.implicitConversions

abstract class StyleFactory {

  type U = scala.xml.Node

  /*
   * Load all style definition into a `doc` style document
   * given the initiial `rootStyle` node.
   */
  def loadFromStyleDoc(rootStyle: scala.xml.Node, doc: DocumentStyle.Aux[U])(implicit logger: Logger): DocumentStyle.Aux[U]

  /*
   * Parse a single style node and return
   * a validated and translated representation of it.
   */
  protected def parseStyle(node: scala.xml.Node, doc: DocumentStyle)(implicit logger: Logger): Option[Style]

}

class StyleFactoryImpl extends StyleFactory {

  import StyleFactoryImpl._

  def loadFromStyleDoc(rootStyle: Node, doc: DocumentStyle.Aux[U])(implicit logger: Logger): DocumentStyle.Aux[U] = {
    (for {
      docWithStyles <- parseGroupNode(rootStyle \!! OdtTags.Styles, doc)
      docWithAutoStyles <- parseGroupNode(rootStyle \!! OdtTags.AutomaticStyle, docWithStyles)
    } yield docWithAutoStyles) getOrElse (doc)
  }

  private def parseGroupNode(node: Option[Node], doc: DocumentStyle.Aux[U])(implicit logger: Logger): Option[DocumentStyle.Aux[U]] =
    node match {
      case None       => Some(doc)
      case Some(node) =>
        Some((node.child).foldLeft(doc) { case (doc, node) =>
          (for {
            name      <- node.attributes.getTag(OdtTags.StyleName)
            styleInfo <- parseStyle(node, doc)
            styleId   <- StyleId.fromNode(node)
          } yield (styleId, styleInfo) +: doc) getOrElse doc
        })
    }

  protected def parseStyle(styleNode: Node, doc: DocumentStyle)(implicit logger: Logger): Option[Style] = {
    if (invalidNodes.contains(styleNode.xmlTag)) {
      logger.info(s"Ignoring node $styleNode")
      None
    } else {
      val id = StyleId.fromNode(styleNode)
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

      val textProps = styleNode \!! OdtTags.StyleTProps
      val parProps = styleNode \!! OdtTags.StylePProps
      val tableProps = scalaz.Tag.unwrap(
                         First(styleNode \!! (OdtTags.StyleTableColProps)) |+|
                         First(styleNode \!! (OdtTags.StyleTableRowProps)) |+|
                         First(styleNode \!! (OdtTags.StyleTableCellProps)) |+|
                         First(styleNode \!! (OdtTags.StyleTableProps)))

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

      object toMap {
        def apply(props: Props): toMap = new toMap(props)
      }

      val mappingFun = toMap((textProps, parProps, tableProps))
      import mappingFun._

      val styleMap = Style.styleProperties.map(mappingFun)
      id map (styleId =>
        Style.typesafeStyle(styleMap, styleId, parentStyle.map(_.id), tpe))
    }
  }

  private def parseListStyle(styleNode: Node): Option[Style] = ???

}

object StyleFactory {

  // Returns a default implementation of the style factory
  def apply(): StyleFactory = _instance

  private lazy val _instance = new StyleFactoryImpl

}

object StyleFactoryImpl {

  final val invalidNodes =
    Seq(
      OdtTags.StyleDefault,
      OdtTags.StylePageLayout,
      OdtTags.TextOutlineStyle,
      OdtTags.TextNotesConf,
      OdtTags.TextLineNumConf)

  final val ParagraphFamily = "paragraph"

}
