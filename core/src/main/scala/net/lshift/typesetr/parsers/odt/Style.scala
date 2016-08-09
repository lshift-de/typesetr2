package net.lshift.typesetr
package parsers
package odt

import net.lshift.typesetr.parsers.Repr
import xml.{ Attribute, AttributeKey, FontFamily, Tag }

import scala.xml.Node

abstract class Style { self =>
  def hasAttribute(key: AttributeKey): Boolean
  def attribute(key: AttributeKey): Option[Attribute]
  def fontFamily: Option[FontFamily]
  def marginLeft: Option[Int]
  def textIdent: Option[Int]
  def tpe: Option[Tag] // or 'tag'

  def update(tpe0: Option[Tag] = self.tpe,
             fontFamily0: Option[FontFamily] = fontFamily): self.type =
    ???
}

object Style {

  def apply(keyVals: Map[String, String],
            textIdent: Int,
            marginLeft: Int): Style =
    new StyleImpl(keyVals, textIdent, marginLeft)

  class StyleImpl(mmap: Map[String, String],
                  textIdent0: Int,
                  marginLeft0: Int) extends Style {

    def hasAttribute(key: AttributeKey): Boolean =
      mmap.contains(key.key)

    def textIdent: Option[Int] =
      Some(textIdent0)

    def marginLeft: Option[Int] =
      Some(marginLeft0)

    def tpe: Option[Tag] =
      None

    def attribute(key: AttributeKey): Option[Attribute] =
      mmap.get(key.key) map (v => Attribute(key, v))

    def fontFamily: Option[FontFamily] =
      None
  }

  lazy val empty: Style =
    new Style() {
      def hasAttribute(key: AttributeKey): Boolean = false

      def textIdent: Option[Int] = None

      def marginLeft: Option[Int] = None

      def tpe: Option[Tag] = None

      def attribute(key: AttributeKey): Option[Attribute] = None

      def fontFamily: Option[FontFamily] = None
    }
}

