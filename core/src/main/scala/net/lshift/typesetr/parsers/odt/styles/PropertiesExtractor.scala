package net.lshift.typesetr
package parsers.odt.styles

import xml.XmlAttribute

import scala.xml.Node
import scalaz.Tags.First
import scalaz.Scalaz._

import scala.language.implicitConversions

/**
 * Extracts a value of the `name` property from the appropriate
 * properties node, `props`.
 */
abstract class PropertiesExtractor(props: Props) {
  def extract(name: XmlAttribute): Option[String]
}

/**
 * Property nodes store different kind of properties.
 *
 * PropertiesExtractorFactory abstract over how such
 * properties are internally represented and returns
 * a simple interface for extracting their values.
 */
abstract class PropertiesExtractorFactory {
  def build(ps: Props): PropertiesExtractor
}

/**
 * Currently the properties can be coming either from the styles that
 * define text, paragraphs, tables, or a mixture of the former.
 */
object PropertiesExtractor {

  def text: PropertiesExtractorFactory = textB

  private lazy val textB =
    new PropertiesExtractorFactory {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlAttribute): Option[String] =
            ps.textProps.flatMap(_.attributes.getTag(name))
        }
    }

  def paragraph: PropertiesExtractorFactory = paragraphB

  private lazy val paragraphB =
    new PropertiesExtractorFactory {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlAttribute): Option[String] =
            ps.parProps.flatMap(_.attributes.getTag(name))
        }
    }

  def table: PropertiesExtractorFactory = tableB

  private lazy val tableB =
    new PropertiesExtractorFactory {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlAttribute): Option[String] =
            ps.tableProps.flatMap(_.attributes.getTag(name))
        }
    }

  def mixed: PropertiesExtractorFactory = mixedB

  private lazy val mixedB =
    new PropertiesExtractorFactory {
      def build(ps: Props): PropertiesExtractor =
        new PropertiesExtractor(ps) {
          def extract(name: XmlAttribute): Option[String] =
            scalaz.Tag.unwrap(
              First(ps.textProps.flatMap(_.attributes.getTag(name))) |+|
                First(ps.parProps.flatMap(_.attributes.getTag(name))))
        }
    }

}