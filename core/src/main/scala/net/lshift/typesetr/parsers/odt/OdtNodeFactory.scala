package net.lshift.typesetr
package parsers
package odt

import Repr._
import xml.{ Attribute, Tag }

import scala.xml.Text

class OdtNodeFactory extends NodeFactory {

  type DocNode = scala.xml.Node

  def create(tag: Tag,
             elem: scala.xml.Node,
             children: Seq[Aux[scala.xml.Node]] = Nil,
             attrs: List[Attribute] = Nil,
             contents: Option[String] = None): Aux[scala.xml.Node] =
    OdtNodeRepr(elem, children, tag, contents, attrs)

  def copy(children: Seq[Aux[DocNode]])(repr: Aux[DocNode]) =
    OdtNodeRepr(repr.source, children, repr.tag, repr.contents, repr.attr)

  def textNode(text: String): scala.xml.Node =
    Text(text)

}
