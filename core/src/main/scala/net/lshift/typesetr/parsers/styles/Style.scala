package net.lshift.typesetr
package parsers
package styles

import util.ValOfUnit
import xml.attributes._

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

}
