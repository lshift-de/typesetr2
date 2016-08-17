package net.lshift.typesetr
package parsers.odt.styles

import net.lshift.typesetr.xml.attributes._

/*
 * Defines a mapping between the different style properties
 * and their respective validators.
 *
 * Validators expect a String value that a) can be validated
 * and b) translated into a proper Style Property object
 *
 */
object StylePropertyValidators extends StylePropertyValidators {

  class TagValidators[K, V]

  implicit def keyToValidator[K <: StylePropKey](implicit k: K): TagValidators[K, StyleValidator.Aux[k.Result]] =
    new TagValidators[K, StyleValidator.Aux[k.Result]]

  final val map = shapeless.HMap[TagValidators](
    /*StylePropKey.Tpe -> StyleValidator.propsList(List[StyleType](TitleStyleTpe, SubTitleStyleTpe,
      HeadingStyle.h1, HeadingStyle.h2, HeadingStyle.h3, HeadingStyle.h4,
      HeadingStyle.h5, HeadingStyle.h6,
      ListStyleTpe, // can"t use "ol"", "ul", because we"ll find out later
      PStyleTpe, SpanStyleTpe, TableTpeStyle, TableRowTpeStyle,
      TableCellTpeStyle, TableColTpeStyle  // ITag.FOOTNOTE ?!?
    ))*/
    StylePropKey.FontFamily -> StyleValidator.all[FontFamily](opt(_)),
    StylePropKey.FontSeize -> StyleValidator.r(StylePropKey.FontSeize.regex, StylePropKey.FontSeize.convert, required = false),
    StylePropKey.FontWeight -> StyleValidator.list[FontWeight](List(FontWeight.Bold, FontWeight.Normal), defaultToV[FontWeight] _, required = false),
    StylePropKey.FontStyleProp -> StyleValidator.list[FontStyle](List(FontStyle.Italic, FontStyle.Normal), defaultToV[FontStyle] _, required = false),
    StylePropKey.Underline -> StyleValidator.list[Underline](List(Underline.Solid, Underline.None), eqTo[Underline](Underline.Solid) _, required = false),
    StylePropKey.LineThrough -> StyleValidator.list[LineThrough](List(LineThrough.Solid, LineThrough.None), eqTo[LineThrough](LineThrough.Solid) _, required = false),
    StylePropKey.Color -> StyleValidator.r(Color.rgbR, defaultTo[xml.attributes.Color](Color("#000000")), required = false),
    StylePropKey.BackgroundColor -> StyleValidator.r(Color.rgbR, defaultTo[xml.attributes.Color](Color("#ffffff")), required = false),
    StylePropKey.TextPosition -> StyleValidator.r[TextPosition]("(sub|super)\b".r, (x: String) => Some(x.split('\b')(0)), required = false),
    StylePropKey.TextAlign -> StyleValidator.list(TextAlign.all, StylePropKey.TextAlign.toResult, required = false),
    StylePropKey.LineHeight -> StyleValidator.r(StylePropKey.LineHeight.regex, StylePropKey.LineHeight.convert, required = false),
    StylePropKey.MarginLeft -> StyleValidator.r(StylePropKey.MarginLeft.regex, StylePropKey.MarginLeft.convert, required = false),
    StylePropKey.TextIndent -> StyleValidator.r(StylePropKey.TextIndent.regex, StylePropKey.TextIndent.convert, required = false),
    StylePropKey.ParBreak-> StyleValidator.list(ParBreak.all, defaultTo[ParBreak](ParBreak.Auto), required = false),
    StylePropKey.SubListStyles -> StyleValidator.all[String](_ => None),
    StylePropKey.MinHeight -> StyleValidator.r(StylePropKey.MinHeight.regex, StylePropKey.MinHeight.convert, required = false),
    StylePropKey.ColWidth -> StyleValidator.r(StylePropKey.ColWidth.regex, StylePropKey.ColWidth.convert, required = false)
  )

}

class StylePropertyValidators {

  protected final def defaultTo[T](v: T)(implicit conv: String => Option[T]): String => Option[T] =
  { (x: String) => if (x == v) None else conv(x) }

  protected final def defaultToV[T](implicit conv: String => Option[T]): String => Option[T] =
  { (x: String) => conv(x) }

  // TODO: Unlike the original version
  // we actually set the style, even if it is the default one.
  // Need to verify if that is still fine (for example line-through)
  protected final def eqTo[T](v: T)(implicit conv: String => Option[T]): String => Option[T] =
    (x: String) => conv(x)
  /* In the original code, this was more close to this
  conv(x) match {
    case res@Some(x) if x == v => res
    case _ => None
  }*/

  protected def opt[T](x: String)(implicit f: String => T): Option[T] = Some(f(x))

}

