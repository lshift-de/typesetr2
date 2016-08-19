package net.lshift.typesetr
package parsers
package odt

import xml.{ Attribute, Tag }

case class OdtNodeRepr(
  source: scala.xml.Node,
  body: Seq[Repr.Aux[scala.xml.Node]],
  tag: Tag,
  contents: Option[String],
  attr: List[Attribute]) extends Repr {

  assert(contents.nonEmpty || (tag != Tag.textTag))

  type R = scala.xml.Node
  type BodyTpe = Repr.Aux[scala.xml.Node]
}
