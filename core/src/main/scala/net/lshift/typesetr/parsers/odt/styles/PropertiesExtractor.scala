package net.lshift.typesetr
package parsers.odt.styles

import xml.XmlTag

import scala.xml.Node
import scalaz.Tags.First
import scalaz.Scalaz._

import scala.language.implicitConversions

case class Props(textProps: Option[scala.xml.Node],
                 parProps: Option[scala.xml.Node],
                 tableProps: Option[scala.xml.Node])

object Props {
  implicit def toProps(ps: (Option[scala.xml.Node], Option[scala.xml.Node], Option[scala.xml.Node])): Props =
    Props(ps._1, ps._2, ps._3)
}

// Extracts a node of the given `name` from the appropriate
// properties node, `props`.
abstract class PropertiesExtractor(val props: Props) {
  def extract(name: XmlTag): Option[String]
}

// There are different kinds of nodes that store properties nodes.
// Some of them have internal nodes that have the same name, so
// builders instantiate the extractors where they will be looked up.
abstract class PropertiesExtractorBuilder {
  def build(ps: Props): PropertiesExtractor
}

// Currently the properties can be coming either from the styles that
// define text, paragraphs, tables, or a mixture of the former.
object PropertiesExtractor {

  def text: PropertiesExtractorBuilder =
    textB

  private lazy val textB =
    new PropertiesExtractorBuilder {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlTag): Option[String] =
            ps.textProps.flatMap(_.attributes.getTag(name))
        }
    }

  def paragraph: PropertiesExtractorBuilder =
    paragraphB

  private lazy val paragraphB =
    new PropertiesExtractorBuilder {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlTag): Option[String] =
            ps.parProps.flatMap(_.attributes.getTag(name))
        }
    }

  def table: PropertiesExtractorBuilder =
    tableB

  private lazy val tableB =
    new PropertiesExtractorBuilder {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlTag): Option[String] =
            ps.tableProps.flatMap(_.attributes.getTag(name))
        }
    }

  def mixed: PropertiesExtractorBuilder =
    mixedB

  private lazy val mixedB =
    new PropertiesExtractorBuilder {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlTag): Option[String] =
            scalaz.Tag.unwrap(
              First(ps.textProps.flatMap(_.attributes.getTag(name))) |+|
                First(ps.parProps.flatMap(_.attributes.getTag(name))))
        }
    }

}