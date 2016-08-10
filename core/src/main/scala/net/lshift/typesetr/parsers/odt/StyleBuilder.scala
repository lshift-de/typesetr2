package net.lshift.typesetr
package parsers
package odt

import net.lshift.typesetr.parsers.odt.styles.StyleValidator
import net.lshift.typesetr.xml.attributes._

import scala.xml.Node

import shapeless.HMap

abstract class StyleBuilder {
  def loadFromStyleDoc(rootStyle: scala.xml.Node): Style

  protected def parseStyle(node: scala.xml.Node): Option[Style]
}

class StyleBuilderImpl extends StyleBuilder {
  def loadFromStyleDoc(rootStyle: Node): Style = ???

  protected def parseStyle(node: Node): Option[Style] = ???
}

object StyleBuilder {

  class TagsValidators[K, V]

  implicit def keyToValidator[K <: StyleKey](implicit k: K): TagsValidators[K, StyleValidator.Aux[k.Result]] =
    ???

  import xml.{InternalTags => ITag}

  def opt[T](x: String)(implicit f: String => T): Option[T] =
    Some(f(x))


  private final val validators = HMap[TagsValidators](
     StyleKey.Tpe -> StyleValidator.list(List(ITag.TITLE, ITag.SUBTITLE,
      ITag.H1, ITag.H2, ITag.H3, ITag.H4, ITag.H5, ITag.H6,
      ITag.LIST, // can"t use "ol"", "ul", because we"ll find out later
      ITag.P, ITag.SPAN, ITag.TABLE, ITag.TR, ITag.TD, ITag.COL,
      ITag.FOOTNOTE)),
     StyleKey.FontFamily -> StyleValidator.all[FontFamily](opt(_)),
     StyleKey.FSeize -> StyleValidator.r(StyleKey.FSeize.regex, StyleKey.FSeize.convert),
     StyleKey.FontWeight -> StyleValidator.list[FontWeight](List(FontWeight.Bold, FontWeight.Normal), defaultToV[FontWeight] _),
     StyleKey.FontStyle -> StyleValidator.list[FontStyle](List(FontStyle.Italic, FontStyle.Normal), defaultToV[FontStyle] _),
     StyleKey.Underline -> StyleValidator.list[Underline](List(Underline.Solid, Underline.None), eqTo[Underline](Underline.Solid) _),
     StyleKey.LineThrough -> StyleValidator.list[LineThrough](List(LineThrough.Solid, LineThrough.None), eqTo[LineThrough](LineThrough.Solid) _),
     StyleKey.Color -> StyleValidator.r(Color.rgbR, defaultTo[xml.attributes.Color]("#000000")),
     StyleKey.BackgroundColor -> StyleValidator.r(Color.rgbR, defaultTo[xml.attributes.Color]("#ffffff")),
     StyleKey.TextPosition -> StyleValidator.r("(sub|super)\b".r, (x: String) => Some(x.split('\b')(0))),
     StyleKey.TextAlign -> StyleValidator.list(TextAlign.all, StyleKey.TextAlign.convert _),
     StyleKey.LineHeight -> StyleValidator.r(StyleKey.LineHeight.regex, StyleKey.LineHeight.convert),
     StyleKey.MarginLeft -> StyleValidator.r(StyleKey.MarginLeft.regex, StyleKey.MarginLeft.convert),
     StyleKey.TextIndent -> StyleValidator.r(StyleKey.TextIndent.regex, StyleKey.TextIndent.convert),
     StyleKey.ParBreak-> StyleValidator.list(ParBreak.all, defaultTo[ParBreak](ParBreak.Auto) ),
     StyleKey.SubListStyles -> StyleValidator.all[String](_ => None),
     StyleKey.MinHeight -> StyleValidator.r(StyleKey.MinHeight.regex, StyleKey.MinHeight.convert)
  )

  private final def defaultTo[T](v: T)(implicit conv: String => T): String => Option[T] =
    { (x: String) => if (x == v) None else Some(conv(x)) }

  private final def defaultToV[T](implicit conv: String => T): String => Option[T] =
    { (x: String) => Some(conv(x)) }


  private final def eqTo[T](v: T)(implicit conv: String => T): String => Option[T] =
    { (x: String) => if (conv(x) == v) Some(v) else None }

  private final val omittableStyleTags =
    Seq(
      OdtTags.StyleDefault,
      OdtTags.StylePageLayout,
      OdtTags.TextOutlineStyle,
      OdtTags.TextNotesConf,
      OdtTags.TextLineNumConf)

}

sealed abstract class StyleKey { type Result }

object StyleKey {

  trait RegexConverter {
    self: StyleKey =>

    def regex: scala.util.matching.Regex

    protected def toResult: String => self.Result

    def convert(x: String): Option[self.Result] =
      regex.findFirstMatchIn(x).map(x => toResult(x.matched))

  }

  case object Tpe extends StyleKey {
    type Result = xml.Tag
    implicit val TpeTag: Tpe.type = this
  }

  case object FontFamily extends StyleKey {
    type Result = xml.attributes.FontFamily
    implicit val FFamilyTag: FontFamily.type = this
  }

  case object FSeize extends StyleKey with RegexConverter {
    type Result = Int
    val regex = "\\d+(.\\d+)?(pt|\\%)".r
    implicit val FSeizeTag: FSeize.type = this
    def toResult = (x: String) => x.toInt
  }

  case object FontWeight extends StyleKey {
    type Result = xml.attributes.FontWeight
    implicit val FWeightTag: FontWeight.type = this
  }

  case object FontStyle extends StyleKey {
    type Result = xml.attributes.FontStyle
    implicit val FStyleTag: FontStyle.type = this
  }

  case object Underline extends StyleKey {
    type Result = xml.attributes.Underline
    implicit val UnderlineTag: Underline.type = this
  }

  case object LineThrough extends StyleKey {
    type Result = xml.attributes.LineThrough
    implicit val LineThroughTag: LineThrough.type = this
  }

  case object Color extends StyleKey {
    type Result = xml.attributes.Color
    implicit val ColorTag: Color.type = this
  }

  case object BackgroundColor extends StyleKey {
    type Result = xml.attributes.Color
    implicit val BackgroundColorTag: BackgroundColor.type = this
  }

  case object TextPosition extends StyleKey {
    type Result = String // TODO
    implicit val TextPositionTag: TextPosition.type = this
  }

  case object TextAlign extends StyleKey {
    type Result = xml.attributes.TextAlign
    implicit val TextAlignTag: TextAlign.type = this
    def convert(x: String): Option[Result] = ???
  }

  case object LineHeight extends StyleKey with RegexConverter {
    type Result = Int
    val regex = "-?\\d+([.]\\d*)?%".r
    implicit val LineHeightTag: LineHeight.type = this
    def toResult = (x: String) => x.toInt
  }

  case object MarginLeft extends StyleKey with RegexConverter {
    type Result = Int
    val regex = "-?\\d+([.]\\d+)?cm".r
    implicit val MarginLeftTag: MarginLeft.type = this
    def toResult = (x: String) => x.toInt
  }

  case object TextIndent extends StyleKey with RegexConverter {
    type Result = Int
    implicit val TextIndentTag: TextIndent.type = this

    val regex = "-?\\d+([.]\\d+)?cm".r
    def toResult = (x: String) => x.toInt
  }

  case object ParBreak extends StyleKey {
    type Result = xml.attributes.ParBreak
    implicit val ParBreakTag: ParBreak.type = this
  }

  case object SubListStyles extends StyleKey {
    type Result = String // TODO
    implicit val SubListStylesTag: SubListStyles.type = this
  }

  case object MinHeight extends StyleKey with RegexConverter {
    type Result = Int
    val regex = "\\d+(.\\d+)?cm".r
    implicit val MinHeightTag: MinHeight.type = this
    def toResult = (x: String) => x.toInt
  }
}
