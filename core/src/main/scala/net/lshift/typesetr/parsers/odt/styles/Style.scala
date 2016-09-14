package net.lshift.typesetr
package parsers.odt.styles

import xml.attributes._
import net.lshift.typesetr.xml.{ Attribute, AttributeKey, Tag }

import shapeless.labelled.KeyTag
import shapeless._

import syntax.singleton._
import record._
import util.ValOfUnit

// TODO: the set of inherited properties from the parent.

abstract class Style { self =>

  def unsafeProperty[T <: StylePropKey](x: T): Option[x.Result]

  def id: StyleId

  def parent: Option[StyleId]

  def tpe: Option[StyleType]

  def fontFamily: Option[FontFamily]

  def fontSize: Option[ValOfUnit]

  def fontWeight: Option[FontWeight]

  def fontStyle: Option[FontStyle]

  def underline: Option[Underline]

  def lineThrough: Option[LineThrough]

  def color: Option[Color]

  def backgrounColor: Option[Color]

  def textPosition: Option[TextPosition]

  def textAlign: Option[TextAlign]

  def lineHeight: Option[ValOfUnit]

  def marginLeft: Option[ValOfUnit]

  def textIndent: Option[ValOfUnit]

  def parBreak: Option[ParBreak]

  def minHeight: Option[ValOfUnit]

  def colWidth: Option[ValOfUnit]

  def source: Option[AnyRef]

  override def toString: String = {
    val props = List(printProp("type", tpe),
      printProp("font-family", fontFamily),
      printProp("font-size", fontSize),
      printProp("font-weight", fontWeight),
      printProp("font-style", fontStyle),
      printProp("underline", underline),
      printProp("line-through", lineThrough),
      printProp("color", color),
      printProp("backgroun-color", backgrounColor),
      printProp("text-position", textPosition),
      printProp("text-align", textAlign),
      printProp("line-height", lineHeight),
      printProp("margin-left", marginLeft),
      printProp("text-indent", textIndent),
      printProp("paragraph-break", parBreak),
      printProp("minimal-height", minHeight),
      printProp("column-width", colWidth))
    s"""|Style '${id}':${parent.map(p => s"\n| > Parent-style: $p").getOrElse("")}
        ${props.flatten.mkString("")}""".stripMargin
  }

  private def printProp[T](name: String, value: Option[T]): Option[String] =
    value map (v => s"| - $name: $v\n")

}

object Style {

  implicit class StyleOps(val x: Style) extends AnyVal {

    def propertiesEmpty: Boolean =
      x.fontSize.isEmpty && x.fontWeight.isEmpty && x.fontStyle.isEmpty &&
        x.underline.isEmpty && x.lineThrough.isEmpty && x.color.isEmpty &&
          x.backgrounColor.isEmpty && x.textPosition.isEmpty && x.textAlign.isEmpty &&
            x.lineHeight.isEmpty && x.marginLeft.isEmpty && x.textIndent.isEmpty &&
              x.parBreak.isEmpty && x.minHeight.isEmpty && x.colWidth.isEmpty

    def isReducible: Boolean =
      x.fontFamily.map(_ == FontFamily.apply("text")).getOrElse(true) && x.propertiesEmpty

  }


  abstract class DefaultStyle extends Style { self =>

    protected def property[T <: StylePropKey](x: T): Option[x.Result]

    def id: StyleId

    def parent: Option[StyleId]

    def tpe: Option[StyleType] =
      property(StylePropKey.Tpe)

    def fontFamily: Option[FontFamily] =
      property(StylePropKey.FontFamily)

    def fontSize: Option[ValOfUnit] =
      property(StylePropKey.FontSeize)

    def fontWeight: Option[FontWeight] =
      property(StylePropKey.FontWeight)

    def fontStyle: Option[FontStyle] =
      property(StylePropKey.FontStyleProp)

    def underline: Option[Underline] =
      property(StylePropKey.Underline)

    def lineThrough: Option[LineThrough] =
      property(StylePropKey.LineThrough)

    def color: Option[Color] =
      property(StylePropKey.Color)

    def backgrounColor: Option[Color] =
      property(StylePropKey.BackgroundColor)

    def textPosition: Option[TextPosition] =
      property(StylePropKey.TextPosition)

    def textAlign: Option[TextAlign] =
      property(StylePropKey.TextAlign)

    def lineHeight: Option[ValOfUnit] =
      property(StylePropKey.LineHeight)

    def marginLeft: Option[ValOfUnit] =
      property(StylePropKey.MarginLeft)

    def textIndent: Option[ValOfUnit] =
      property(StylePropKey.TextIndent)

    def parBreak: Option[ParBreak] =
      property(StylePropKey.ParBreak)

    def minHeight: Option[ValOfUnit] =
      property(StylePropKey.MinHeight)

    def colWidth: Option[ValOfUnit] =
      property(StylePropKey.ColWidth)

  }

  type Aux[S <: StylePropKey] = Option[S#Result] with KeyTag[S#V, Option[S#Result]]

  /*
   * Heterogenous list of records representing a mapping between style
   * properties and types of their values
   */
  type MMap =
    Aux[StylePropKey.FontFamily.type] ::
    Aux[StylePropKey.FontSeize.type] ::
    Aux[StylePropKey.FontWeight.type] ::
    Aux[StylePropKey.FontStyleProp.type] ::
    Aux[StylePropKey.Underline.type] ::
    Aux[StylePropKey.LineThrough.type] ::
    Aux[StylePropKey.Color.type] ::
    Aux[StylePropKey.BackgroundColor.type] ::
    Aux[StylePropKey.TextPosition.type] ::
    Aux[StylePropKey.TextAlign.type] ::
    Aux[StylePropKey.LineHeight.type] ::
    Aux[StylePropKey.MarginLeft.type] ::
    Aux[StylePropKey.TextIndent.type] ::
    Aux[StylePropKey.ParBreak.type] ::
    Aux[StylePropKey.ColWidth.type] ::
    Aux[StylePropKey.MinHeight.type] ::
      HNil

  def typesafeStyle(mmap: MMap, id: StyleId,
                    parent: Option[StyleId], tpe: Option[StyleType])(source: scala.xml.Node): Style =
    new RecordsBackedStyleImpl(mmap, id, parent, tpe)(source)

  private class RecordsBackedStyleImpl(mmap: MMap,
                               val id: StyleId,
                               val parent: Option[StyleId],
                               val tpe: Option[StyleType])(source0: scala.xml.Node)
    extends Style {

    def unsafeProperty[T <: StylePropKey](x: T): Option[x.Result] =
      (mmap.toMap[StylePropKey, Any]).get(x).asInstanceOf[Option[Option[x.Result]]].flatten

    protected def property[T <: StylePropKey](x: Witness.Aux[T])(
      implicit sel: ops.record.Selector[MMap, x.T]) : sel.Out = mmap(x)

    def fontFamily: Option[FontFamily] =
      property(StylePropKey.FontFamily)

    def fontSize: Option[ValOfUnit] =
      property(StylePropKey.FontSeize)

    def fontWeight: Option[FontWeight] =
      property(StylePropKey.FontWeight)

    def fontStyle: Option[FontStyle] =
      property(StylePropKey.FontStyleProp)

    def underline: Option[Underline] =
      property(StylePropKey.Underline)

    def lineThrough: Option[LineThrough] =
      property(StylePropKey.LineThrough)

    def color: Option[Color] =
      property(StylePropKey.Color)

    def backgrounColor: Option[Color] =
      property(StylePropKey.BackgroundColor)

    def textPosition: Option[TextPosition] =
      property(StylePropKey.TextPosition)

    def textAlign: Option[TextAlign] =
      property(StylePropKey.TextAlign)

    def lineHeight: Option[ValOfUnit] =
      property(StylePropKey.LineHeight)

    def marginLeft: Option[ValOfUnit] =
      property(StylePropKey.MarginLeft)

    def textIndent: Option[ValOfUnit] =
      property(StylePropKey.TextIndent)

    def parBreak: Option[ParBreak] =
      property(StylePropKey.ParBreak)

    def minHeight: Option[ValOfUnit] =
      property(StylePropKey.MinHeight)

    def colWidth: Option[ValOfUnit] =
      property(StylePropKey.ColWidth)

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


  // We don't want to access the instance of the records because
  // it may not necessarily exist yet.
  // What we have however, is the precise type, which has materialized
  // type information of its expected keys and values.
  // To retrieve the latter we use some implicit magic.
  private class KeysOfRecordMapHelper[H <: HList] {
    def keys[K <: HList]()(implicit keysWitness: shapeless.ops.record.Keys.Aux[H, K]): keysWitness.Out =
      keysWitness()
  }

  lazy val styleProperties = { (new KeysOfRecordMapHelper[MMap]).keys() }
}
