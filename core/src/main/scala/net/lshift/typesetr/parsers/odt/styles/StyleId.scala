package net.lshift.typesetr
package parsers
package odt
package styles

import xml._

import scalaz.Tags.First
import scalaz.Scalaz._

sealed abstract class StyleId {

  def family: Option[String]

  def name: String

  def withThisFamily(name0: String) = StyleId(family, name0)

  override def toString: String =
    family map (f => s"$f:$name") getOrElse (s"none:$name")

}

object StyleId {

  implicit lazy val StyleIdOrder: Ordering[StyleId] = new Ordering[StyleId]() {
    override def compare(x: StyleId, y: StyleId): Int = {
      val fOrder = (x.family.getOrElse("")).compare(y.family.getOrElse(""))
      if (fOrder == 0) x.name.compare(y.name)
      else fOrder
    }
  }

  def apply(family: Option[String], name: String): StyleId = StyleIdImpl(family, name)

  def none: StyleId = None

  def fromNode(node: scala.xml.Node): Option[StyleId] =
    styleFromNode(node, OdtTags.StyleName)

  def forNonStyleNode(node: scala.xml.Node): Option[StyleId] =
    styleFromNode(node, OdtTags.StyleNameAttr)

  private def styleFromNode(node: scala.xml.Node, attr: XmlTag): Option[StyleId] = {
    val family =
      if (node.xmlTag == OdtTags.TextListStyle) scala.None
      else node.attributes.getTag(OdtTags.StyleFamily)

    for {
      name <- scalaz.Tag.unwrap(First(node.attributes.getTag(attr)) |+|
        First(node.attributes.getTag(OdtTags.TableStyleNameAttr)))
    } yield StyleId(family, name)
  }

  private case class StyleIdImpl(family: Option[String], name: String) extends StyleId

  private case object None extends StyleId {
    def family: Option[String] = scala.None
    def name: String = "none"
  }

}