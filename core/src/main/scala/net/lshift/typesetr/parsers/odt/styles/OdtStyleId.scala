package net.lshift.typesetr
package parsers
package odt
package styles

import net.lshift.typesetr.parsers.odt.OdtTags
import net.lshift.typesetr.parsers.styles.StyleId

import xml._

import scalaz.Tags.First
import scalaz.Scalaz._

object OdtStyleId {

  def fromNode(node: scala.xml.Node): Option[StyleId] =
    styleFromNode(node, OdtTags.StyleName)

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
