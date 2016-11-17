package net.lshift.typesetr
package parsers
package odt
package styles

import net.lshift.typesetr.parsers.odt.OdtTags
import net.lshift.typesetr.parsers.styles.StyleId

import xml._

import scalaz.Tags.First
import scalaz.Scalaz._

/**
 * Utility functions that infer the style's identifier from the ODT element
 */
object OdtStyleId {

  /**
   * Return the name of the style from the document's meta element
   *
   * @param node style's definition in ODT
   * @return style's unique identifier, or none if invalid
   */
  def fromNode(node: scala.xml.Node): Option[StyleId] =
    styleFromNode(node, OdtTags.StyleName)

  /**
   * Return the name of the style for a document's body element
   *
   * @param node the element from the ODT's body
   * @return style's unique identifier, or none if the information is missing
   */
  def forNonStyleNode(node: scala.xml.Node): Option[StyleId] =
    styleFromNode(node, OdtTags.StyleNameAttr)

  private def styleFromNode(node: scala.xml.Node, attr: XmlAttribute): Option[StyleId] = {
    val family =
      if (node.xmlTag == OdtTags.TextListStyle) scala.None
      else node.attributes.getTag(OdtTags.StyleFamily)

    for {
      name <- scalaz.Tag.unwrap(First(node.attributes.getTag(attr)) |+|
        First(node.attributes.getTag(OdtTags.TableStyleNameAttr)))
    } yield StyleId(family, name)
  }

}
