package net.lshift.typesetr
package parsers
package odt.styles

import net.lshift.typesetr.parsers.styles.{StylePropKey, Style, StyleId, StyleType}
import xml.attributes._
import net.lshift.typesetr.xml.{ Attribute, AttributeKey, Tag }

import shapeless.labelled.KeyTag
import shapeless._

import syntax.singleton._
import record._
import util.ValOfUnit

// TODO: the set of inherited properties from the parent.


object OdtStyle {

  abstract class DefaultStyle extends Style { self =>

    protected def property[T <: StylePropKey](x: T): Option[x.Result]

    def id: StyleId

    def parent: Option[StyleId]

    def tpe: Option[StyleType] =
      property(OdtStylePropKeys.Tpe)

    def fontFamily: Option[FontFamily] =
      property(OdtStylePropKeys.FontFamily)

    def fontSize: Option[ValOfUnit] =
      property(OdtStylePropKeys.FontSeize)

    def fontWeight: Option[FontWeight] =
      property(OdtStylePropKeys.FontWeight)

    def fontStyle: Option[FontStyle] =
      property(OdtStylePropKeys.FontStyleProp)

    def underline: Option[Underline] =
      property(OdtStylePropKeys.Underline)

    def lineThrough: Option[LineThrough] =
      property(OdtStylePropKeys.LineThrough)

    def color: Option[Color] =
      property(OdtStylePropKeys.Color)

    def backgrounColor: Option[Color] =
      property(OdtStylePropKeys.BackgroundColor)

    def textPosition: Option[TextPosition] =
      property(OdtStylePropKeys.TextPosition)

    def textAlign: Option[TextAlign] =
      property(OdtStylePropKeys.TextAlign)

    def lineHeight: Option[ValOfUnit] =
      property(OdtStylePropKeys.LineHeight)

    def marginLeft: Option[ValOfUnit] =
      property(OdtStylePropKeys.MarginLeft)

    def textIndent: Option[ValOfUnit] =
      property(OdtStylePropKeys.TextIndent)

    def parBreak: Option[ParBreak] =
      property(OdtStylePropKeys.ParBreak)

    def minHeight: Option[ValOfUnit] =
      property(OdtStylePropKeys.MinHeight)

    def colWidth: Option[ValOfUnit] =
      property(OdtStylePropKeys.ColWidth)

  }

  def typesafeStyle(mmap: TextMMap, id: StyleId,
                    parent: Option[StyleId], tpe: Option[StyleType])(source: scala.xml.Node): Style =
    new RecordsBackedStyleImpl(mmap, id, parent, tpe)(source)

  private class RecordsBackedStyleImpl(mmap: TextMMap,
                               val id: StyleId,
                               val parent: Option[StyleId],
                               val tpe: Option[StyleType])(source0: scala.xml.Node)
    extends Style {

    def unsafeProperty[T <: StylePropKey](x: T): Option[x.Result] =
      (mmap.toMap[StylePropKey, Any]).get(x).asInstanceOf[Option[Option[x.Result]]].flatten

    protected def property[T <: StylePropKey](x: Witness.Aux[T])(
      implicit sel: ops.record.Selector[TextMMap, x.T]) : sel.Out = mmap(x)

    def fontFamily: Option[FontFamily] =
      property(OdtStylePropKeys.FontFamily)

    def fontSize: Option[ValOfUnit] =
      property(OdtStylePropKeys.FontSeize)

    def fontWeight: Option[FontWeight] =
      property(OdtStylePropKeys.FontWeight)

    def fontStyle: Option[FontStyle] =
      property(OdtStylePropKeys.FontStyleProp)

    def underline: Option[Underline] =
      property(OdtStylePropKeys.Underline)

    def lineThrough: Option[LineThrough] =
      property(OdtStylePropKeys.LineThrough)

    def color: Option[Color] =
      property(OdtStylePropKeys.Color)

    def backgrounColor: Option[Color] =
      property(OdtStylePropKeys.BackgroundColor)

    def textPosition: Option[TextPosition] =
      property(OdtStylePropKeys.TextPosition)

    def textAlign: Option[TextAlign] =
      property(OdtStylePropKeys.TextAlign)

    def lineHeight: Option[ValOfUnit] =
      property(OdtStylePropKeys.LineHeight)

    def marginLeft: Option[ValOfUnit] =
      property(OdtStylePropKeys.MarginLeft)

    def textIndent: Option[ValOfUnit] =
      property(OdtStylePropKeys.TextIndent)

    def parBreak: Option[ParBreak] =
      property(OdtStylePropKeys.ParBreak)

    def minHeight: Option[ValOfUnit] =
      property(OdtStylePropKeys.MinHeight)

    def colWidth: Option[ValOfUnit] =
      property(OdtStylePropKeys.ColWidth)

    def source: Option[AnyRef] =
      if (source0 != null) Some(source0) else None

  }

  lazy val empty: Style =
    new DefaultStyle {

      def property[T <: StylePropKey](x: T): Option[x.Result] = None

      def unsafeProperty[T <: StylePropKey](x: T): Option[x.Result] = None

      def parent: Option[StyleId] = None

      def id: StyleId = StyleId.none

      def source: Option[AnyRef] = None

    }


  type Aux[S <: StylePropKey] = Option[S#Result] with KeyTag[S#V, Option[S#Result]]

  /**
    * Heterogeneous list of records representing a mapping between text style
    * properties and types of their values
    */
  private type TextMMap =
    Aux[OdtStylePropKeys.FontFamily.type] ::
    Aux[OdtStylePropKeys.FontSeize.type] ::
    Aux[OdtStylePropKeys.FontWeight.type] ::
    Aux[OdtStylePropKeys.FontStyleProp.type] ::
    Aux[OdtStylePropKeys.Underline.type] ::
    Aux[OdtStylePropKeys.LineThrough.type] ::
    Aux[OdtStylePropKeys.Color.type] ::
    Aux[OdtStylePropKeys.BackgroundColor.type] ::
    Aux[OdtStylePropKeys.TextPosition.type] ::
    Aux[OdtStylePropKeys.TextAlign.type] ::
    Aux[OdtStylePropKeys.LineHeight.type] ::
    Aux[OdtStylePropKeys.MarginLeft.type] ::
    Aux[OdtStylePropKeys.TextIndent.type] ::
    Aux[OdtStylePropKeys.ParBreak.type] ::
    Aux[OdtStylePropKeys.ColWidth.type] ::
    Aux[OdtStylePropKeys.MinHeight.type] ::
    HNil


  // We don't want to access the instance of the records because
  // it may not necessarily exist yet.
  // What we have however, is the precise type, which has materialized
  // type information of its expected keys and values.
  // To retrieve the latter we use some implicit magic.
  private class KeysOfRecordMapHelper[H <: HList] {
    def keys[K <: HList]()(implicit keysWitness: shapeless.ops.record.Keys.Aux[H, K]): keysWitness.Out =
      keysWitness()
  }

  lazy val textStyleProperties = { (new KeysOfRecordMapHelper[TextMMap]).keys() }

}
