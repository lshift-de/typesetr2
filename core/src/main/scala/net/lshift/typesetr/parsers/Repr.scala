package net.lshift.typesetr
package parsers

import net.lshift.typesetr.parsers.Repr.Aux
import xml.{ AttributeKey, Attribute, Tag }

import scala.annotation.tailrec
import scala.xml.{ Text, Node, MetaData, Elem }

import scala.language.implicitConversions

abstract class Repr {
  self =>

  type R

  type BodyTpe <: Repr.Aux[R]

  type V = String

  def tag: Tag

  def attr: List[Attribute]

  def contents: Option[V]

  def source: R

  def body: Seq[BodyTpe]

  def isEmpty: Boolean = body.isEmpty

  def hasContents: Boolean = contents.nonEmpty

}

object Repr {

  type Aux[T] = Repr { type R = T }

  def makeElem[T](tag: Tag,
                  body: Seq[Repr.Aux[T]])(
    implicit source: T, builder: NodeFactory[T]): Repr.Aux[T] =
    builder.create(tag, source, body)

  def makeElem[T](tag: Tag,
                  contents: String)(
    implicit source: T, builder: NodeFactory[T]): Repr.Aux[T] =
    builder.create(tag, source, Nil)

  def makeElem[T](tag: Tag,
                  body: Seq[Repr.Aux[T]],
                  attr: List[Attribute])(
    implicit source: T, builder: NodeFactory[T]): Repr.Aux[T] =
    builder.createWithAttributes(tag, source, body, attr)

  def makeTextElem[T](contents0: String, synthetic: Boolean = false)(
    implicit source: T, builder: NodeFactory[T]): Repr.Aux[T] = {
    val t = if (synthetic) Tag.syntheticTextTag else Tag.textTag
    builder.createWithContents(t, source, contents0)
  }


  def optMakeElem[T](tag: Tag, body: Seq[Repr.Aux[T]])(implicit builder: NodeFactory[T]): Option[Seq[Repr.Aux[T]]] =
    if (body.isEmpty) None
    else ???

  def empty[T](implicit builder: ReprEmptyBuilder[T]): Repr.Aux[T] =
    builder.empty()


  implicit class ReprOps(val x: Repr) extends AnyVal {

    def hasTag(tags: List[Tag]): Boolean =
      tags.contains(x)

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

    // TODO: optimize
    def hasAttribute(attr: Attribute): Boolean =
      hasAttribute(List(attr))

    def hasAttribute(attrKey: AttributeKey): Boolean =
      x.attr.find(_.key == attrKey).nonEmpty

    def getAttribute(attrName: String): Option[Attribute] =
      x.attr.find(_.key == attrName)

    def hasAttrWithVal(attrName: String, value: String): Boolean =
      x.attr.find(_.key == attrName).map(_.value == value).getOrElse(false)

    def extractPlainText: Option[String] = x match {
      case TextRepr(text) =>
        Some(text)
      case _ =>
        None
    }

  }
}

object TextRepr {
  def unapply(x: Repr): Option[String] =
    if (x.body.nonEmpty) None else x.contents
}
