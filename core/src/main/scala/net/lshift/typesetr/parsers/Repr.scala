package net.lshift.typesetr
package parsers

import net.lshift.typesetr.parsers.Repr.Aux
import xml.{ AttributeKey, Attribute, Tag }

import scala.annotation.tailrec
import scala.xml.{ Text, Node, MetaData, Elem }

import scala.language.implicitConversions

/**
  * An internal, platform-independent, representation
  * of a single document node.
  */
abstract class Repr {
  self =>

  private final val id = Repr.newNode

  // An underlying, initial type of the node
  type R

  // Types of children
  type BodyTpe <: Repr.Aux[R]

  // An internal tag associated with the node
  def tag: Tag

  // List of attributes to attach to the node
  def attr: List[Attribute]

  // A potentially non-empty text value of the node
  def contents: Option[String]

  // The initial information from which this node
  // has beeen created
  def source: R

  /**
    * Children of the node. Order preserving.
    */
  def body: Seq[BodyTpe]

  /**
    * Does this node have any children?
    */
  def isEmpty: Boolean = body.isEmpty

  /**
    * Does this node have any text contents?
    */
  def hasContents: Boolean = contents.nonEmpty

  override def toString: String =
    s"[$id][$tag]: ${super.toString}"

  def copy(children: Seq[Repr.Aux[R]] = body, source1: R = self.source, attr: List[Attribute] = self.attr)(implicit factory: NodeFactory.Aux[R]): self.type =
    factory.copy(children, source1, attr)(self).asInstanceOf[self.type]

}

object Repr {

  type Aux[T] = Repr { type R = T }

  // TODO: cleanup
  def makeElem[T](tag: Tag,
                  body: Seq[Repr.Aux[T]],
                  contents: Option[String],
                  attrs: List[Attribute])(
    implicit source: T, factory: NodeFactory.Aux[T]): Repr.Aux[T] =
    factory.create(tag, source, body, attrs = attrs, contents = contents)

  def makeTextElem[T](contents0: String, synthetic: Boolean = false)(
    implicit factory: NodeFactory.Aux[T]): Repr.Aux[T] = {
    val t = if (synthetic) Tag.syntheticTextTag else Tag.textTag
    factory.createWithContents(t, factory.textNode(contents0), contents0)
  }

  def empty[T](implicit builder: ReprNullFactory[T]): Repr.Aux[T] =
    builder.empty()

  implicit class ReprOps[T](val x: Repr.Aux[T]) extends AnyVal {

    def hasTag(tags: List[Tag]): Boolean =
      tags.contains(x.tag)

    def hasTag(target: Tag): Boolean =
      hasTag(List(target))

    def hasAttribute(attrs: List[Attribute]): Boolean = {
      def checkAttribute(attrs: List[Attribute]): Boolean =
        attrs match {
          case Nil          => false
          case head :: rest => attrs contains head
        }
      checkAttribute(attrs)
    }

    def hasAttribute(attr: Attribute): Boolean =
      hasAttribute(List(attr))

    def hasAttribute(attrKey: AttributeKey): Boolean =
      x.attr.find(_.key == attrKey).nonEmpty

    def getAttribute(attrName: AttributeKey): Option[Attribute] =
      x.attr.find(_.key == attrName)

    def hasAttrWithVal(attrName: AttributeKey, value: String): Boolean =
      x.attr.find(_.key == attrName).map(_.value == value).getOrElse(false)

    def extractPlainText(implicit info: NodeInfo.Aux[T]): Option[String] = extractPlainText(deep = false)
    def extractPlainText(deep: Boolean)(implicit info: NodeInfo.Aux[T]): Option[String] = x match {
      case TextRepr(text) =>
        Some(text)
      case _ if deep =>
        val txtRepr: Option[String] = info.textRepresentation(x)
        if (txtRepr.isEmpty) {
          val r = x.body flatMap { node => node.tag match {
              case Tag.textTag =>
                node.contents.map(TextRepr.decodeText)
              case _ =>
                // Note: casting to shut up the compiler
                node.extractPlainText(deep)(info.asInstanceOf[NodeInfo.Aux[node.R]])
            }
          }

          if (r.isEmpty) None
          else Some(r.mkString(""))
        } else txtRepr
      case _ =>
        None
    }

  }

  private final var id = 0

  private final def newNode: Int = { id = id + 1; id }

}

object TextRepr {
  def unapply(x: Repr): Option[String] =
    if (x.body.nonEmpty) None else x.contents.map(decodeText)

  private final val encodingMap: Map[String, String] = Map(
    " &nbsp;" -> " "
  )

  def decodeText(s: String): String =
    encodingMap.keys.foldLeft(s) {
      case (s0, key) =>  s0.replaceAllLiterally(key, encodingMap(key))
    }
}
