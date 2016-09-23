package net.lshift.typesetr
package parsers
package odt

import parsers.Repr.Aux
import xml.InternalTags

import scala.xml.{ Node, Text }

class OdtNodeInfo extends NodeInfo {

  type DocNode = scala.xml.Node

  def isText(node: Aux[Node]): Boolean =
    node.source match {
      case _: Text => true
      case _       => false
    }

  def docContent(root: Aux[Node]): List[Repr.Aux[Node]] =
    root.body.find(_.tag == InternalTags.BODY).
      map(_.body(0).body).getOrElse(Nil).toList

  def isContentInBody(node: Repr.Aux[DocNode], nestingLevel: Int): Boolean =
    if (nestingLevel == 0) node.source.xmlTag == OdtTags.Text
    else false

  def canContainContent(node: Repr.Aux[DocNode], nestingLevel: Int): Boolean =
    nestingLevel == 0

  def textRepresentation(node: Repr.Aux[DocNode]): Option[String] = node.source.xmlTag match {
    case OdtTags.S =>
      // todo: recognize the number of whitespaces
      val spaces =
        node.source.attributes.getTag(OdtTags.C).map(_.toInt).getOrElse(1)
      Some(" " * spaces)
    case OdtTags.Tab =>
      // todo: recognize the number of tabs
      ???
    case OdtTags.Linebreak => Some("\n")
    case _                 => None
  }

}
