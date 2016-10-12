package net.lshift.typesetr
package parsers
package odt

import Repr._
import xml.{ Attribute, Tag }

import scala.xml.{ TopScope, Elem, Text }

class OdtNodeFactory extends NodeFactory {

  type DocNode = scala.xml.Node

  def create(tag: Tag,
             elem: scala.xml.Node,
             children: Seq[Aux[scala.xml.Node]] = Nil,
             attrs: List[Attribute] = Nil,
             contents: Option[String] = None): Aux[scala.xml.Node] =
    OdtNodeRepr(elem, children, tag, contents, attrs)

  def copy(children: Seq[Aux[DocNode]], source1: DocNode)(repr: Aux[DocNode]) =
    OdtNodeRepr(source1, children, repr.tag, repr.contents, repr.attr)

  def textNode(text: String): scala.xml.Node =
    Text(text)

  def newLineNode(): scala.xml.Node = {
    new Elem(
      prefix = OdtTags.Linebreak.namespace.short.value,
      label = OdtTags.Linebreak.tag,
      attributes1 = scala.xml.Null,
      scope = TopScope,
      minimizeEmpty = true)
  }

}
