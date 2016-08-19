package net.lshift.typesetr
package parsers
package odt

import Repr._
import xml.{ Attribute, Tag }

import scala.xml.Text

class OdtNodeFactory extends NodeFactory[scala.xml.Node] {

  def create(tag: Tag,
             elem: scala.xml.Node,
             children: Seq[Aux[scala.xml.Node]] = Nil,
             attrs: List[Attribute] = Nil,
             contents: Option[String] = None): Aux[scala.xml.Node] =
    OdtNodeRepr(elem, children, tag, contents, attrs)

  def createWithAttributes(
    tag: Tag,
    elem: scala.xml.Node,
    children: Seq[Aux[scala.xml.Node]],
    attrs: List[Attribute]): Aux[scala.xml.Node] =
    OdtNodeRepr(elem, children, tag, None, attrs)

  def createWithContents(tag: Tag,
                         elem: scala.xml.Node,
                         contents: String): Aux[scala.xml.Node] =
    OdtNodeRepr(elem, Nil, tag, Some(contents), Nil)

  def textNode(text: String): scala.xml.Node =
    Text(text)
}
