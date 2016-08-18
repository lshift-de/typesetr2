package net.lshift.typesetr.parsers

import net.lshift.typesetr.xml.{ Attribute, Tag }

abstract class NodeFactory[T] {

  def create(tag: Tag, elem: T,
             children: Seq[Repr.Aux[T]] = Nil,
             attrs: List[Attribute] = Nil,
             contents: Option[String] = None): Repr.Aux[T]

  def createWithAttributes(tag: Tag, elem: T, children: Seq[Repr.Aux[T]],
                           attrs: List[Attribute]): Repr.Aux[T]

  def createWithContents(tag: Tag, elem: T, contents: String): Repr.Aux[T]

  def textNode(text: String): T
}
