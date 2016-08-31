package net.lshift.typesetr
package parsers

import net.lshift.typesetr.xml.{ Attribute, Tag }

// Interface for format-independent creation of the
// typesetr's internal nodes
abstract class NodeFactory {

  type DocNode

  def create(tag: Tag,
             elem: DocNode,
             children: Seq[Repr.Aux[DocNode]] = Nil,
             attrs: List[Attribute] = Nil,
             contents: Option[String] = None): Repr.Aux[DocNode]

  def createWithAttributes(tag: Tag, elem: DocNode, children: Seq[Repr.Aux[DocNode]],
                           attrs: List[Attribute]): Repr.Aux[DocNode]

  def createWithContents(tag: Tag, elem: DocNode, contents: String): Repr.Aux[DocNode]

  def textNode(text: String): DocNode
}

object NodeFactory {

  type Aux[T] = NodeFactory { type DocNode = T }

  implicit def fromConfig[T](implicit inputConfig: NodeConfigs.WithNode[T]): NodeFactory.Aux[T] =
    inputConfig.nodeFactory

}
