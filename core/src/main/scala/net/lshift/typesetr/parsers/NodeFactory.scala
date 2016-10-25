package net.lshift.typesetr
package parsers

import net.lshift.typesetr.xml.{ Attribute, Tag }

// Typeclass for creating typesetr's representation
// of the original document
abstract class NodeFactory {

  type DocNode

  def create(tag: Tag,
             docNode: DocNode,
             children: Seq[Repr.Aux[DocNode]],
             attrs: List[Attribute] = Nil,
             contents: Option[String] = None): Repr.Aux[DocNode]

  def createWithContents(tag: Tag,
                         docNode: DocNode,
                         contents: String): Repr.Aux[DocNode] =
    create(tag, docNode, Nil, Nil, Some(contents))

  def imgWithCaption(img: Repr.Aux[DocNode], caption: Seq[Repr.Aux[DocNode]]): Repr.Aux[DocNode]

  def paragraphFrom(body: Seq[Repr.Aux[DocNode]], p0: Repr.Aux[DocNode]): Repr.Aux[DocNode]

  def copy(children: Seq[Repr.Aux[DocNode]], source1: DocNode)(repr: Repr.Aux[DocNode]): Repr.Aux[DocNode]

  def textNode(text: String): DocNode

  def newLineNode(): DocNode

}

object NodeFactory {

  type Aux[T] = NodeFactory { type DocNode = T }

  implicit def fromConfig[T](implicit inputConfig: NodeConfigs.WithNode[T]): NodeFactory.Aux[T] =
    inputConfig.nodeFactory

}
