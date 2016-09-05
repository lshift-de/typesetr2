package net.lshift.typesetr
package parsers
package odt
package styles

import xml._

import scalaz.Tags.First
import scalaz.Scalaz._

/**
 * Class representing a unique identifier of a single style.
 *
 * According to the spec, style is always unique when you take into account
 * it's family and name. It should also be searched for this way, except
 * that text nodes only mention the name and assume that no overriding is
 * possible.
 */
sealed abstract class StyleId {

  def family: Option[String]

  def name: String

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

  private def styleFromNode(node: scala.xml.Node, attr: XmlAttribute): Option[StyleId] = {
    val family =
      if (node.xmlTag == OdtTags.TextListStyle) scala.None
      else node.attributes.getTag(OdtTags.StyleFamily)

    for {
      name <- scalaz.Tag.unwrap(First(node.attributes.getTag(attr)) |+|
        First(node.attributes.getTag(OdtTags.TableStyleNameAttr)))
    } yield StyleId(family, name)
  }

  implicit class StyleIdOps(val s: StyleId) extends AnyVal {
    def withThisFamily(name0: String) = StyleId(s.family, name0)
  }

  private case class StyleIdImpl(family: Option[String], name: String) extends StyleId

  private case object None extends StyleId {
    def family: Option[String] = scala.None
    def name: String = "none"
  }

}