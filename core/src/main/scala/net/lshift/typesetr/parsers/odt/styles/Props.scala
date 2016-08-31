package net.lshift.typesetr
package parsers
package odt
package styles

import scala.language.implicitConversions
import scalaz.Tags.First
import scalaz.Scalaz._

/**
 * Encapsulates the logic to access the different kinds of
 * style properties.
 * Currently we assume that the ODT document exposes
 * text, paragraph and table properties.
 */
class Props(styleNode: scala.xml.Node) {

  /**
    * Text properties of the style, if any.
    */
  def textProps: Option[scala.xml.Node] =
    styleNode \!! odt.OdtTags.StyleTProps

  /**
    * Paragraph properties of the style, if any.
    * @return
    */
  def parProps: Option[scala.xml.Node] =
    styleNode \!! odt.OdtTags.StylePProps

  /**
    * Table properties of the style, if any.
    * Returns column, row, cell or table properties, whichever
    * is first, if any.
    *
    * // TODO: we might want revisit that approach (taken from
    *          the original typesetr and merge them together
    */
  def tableProps: Option[scala.xml.Node] =
    scalaz.Tag.unwrap(
      First(styleNode \!! (OdtTags.StyleTableColProps)) |+|
      First(styleNode \!! (OdtTags.StyleTableRowProps)) |+|
      First(styleNode \!! (OdtTags.StyleTableCellProps)) |+|
      First(styleNode \!! (OdtTags.StyleTableProps)))

}

object Props {

  implicit def fromStyleNode(node: scala.xml.Node): Props = new Props(node)

}
