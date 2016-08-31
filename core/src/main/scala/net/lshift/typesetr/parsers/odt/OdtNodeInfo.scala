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

}
