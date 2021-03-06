package net.lshift.typesetr
package parsers.odt.styles

import net.lshift.typesetr.parsers.styles.{StylePropKey, StyleValidator}
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
    /*StylePropKey.Tpe -> StyleValidator.list(StyleType.all)*/
    OdtStylePropKeys.FontFamily       -> StyleValidator.all[FontFamily](opt(_)),
    OdtStylePropKeys.FontSeize        -> StyleValidator.r(OdtStylePropKeys.FontSeize.regex, OdtStylePropKeys.FontSeize.convert, required = false),
    OdtStylePropKeys.FontWeight       -> StyleValidator.list[FontWeight](List(FontWeight.Bold, FontWeight.Normal), defaultToV[FontWeight] _, required = false),
    OdtStylePropKeys.FontStyleProp    -> StyleValidator.list[FontStyle](List(FontStyle.Italic, FontStyle.Normal), defaultToV[FontStyle] _, required = false),
    OdtStylePropKeys.Underline        -> StyleValidator.list[Underline](List(Underline.Solid, Underline.None), eqTo[Underline](Underline.Solid) _, required = false),
    OdtStylePropKeys.LineThrough      -> StyleValidator.list[LineThrough](List(LineThrough.Solid, LineThrough.None), eqTo[LineThrough](LineThrough.Solid) _, required = false),
    OdtStylePropKeys.Color            -> StyleValidator.r(Color.rgbR, defaultTo[xml.attributes.Color](Color("#000000")), required = false),
    OdtStylePropKeys.BackgroundColor  -> StyleValidator.r(Color.rgbR, defaultTo[xml.attributes.Color](Color("#ffffff")), required = false),
    OdtStylePropKeys.TextPosition     -> StyleValidator.r[TextPosition]("(sub|super)\b".r, (x: String) => Some(x.split('\b')(0)), required = false),
    OdtStylePropKeys.TextAlign        -> StyleValidator.list(TextAlign.all, OdtStylePropKeys.TextAlign.toResult, required = false),
    OdtStylePropKeys.LineHeight       -> StyleValidator.r(OdtStylePropKeys.LineHeight.regex, OdtStylePropKeys.LineHeight.convert, required = false),
    OdtStylePropKeys.MarginLeft       -> StyleValidator.r(OdtStylePropKeys.MarginLeft.regex, OdtStylePropKeys.MarginLeft.convert, required = false),
    OdtStylePropKeys.TextIndent       -> StyleValidator.r(OdtStylePropKeys.TextIndent.regex, OdtStylePropKeys.TextIndent.convert, required = false),
    OdtStylePropKeys.ParBreak         -> StyleValidator.list(ParBreak.all, defaultTo[ParBreak](ParBreak.Auto), required = false),
    OdtStylePropKeys.SubListStyles    -> StyleValidator.all[String](_ => None),
    OdtStylePropKeys.MinHeight        -> StyleValidator.r(OdtStylePropKeys.MinHeight.regex, OdtStylePropKeys.MinHeight.convert, required = false),
    OdtStylePropKeys.ColWidth         -> StyleValidator.r(OdtStylePropKeys.ColWidth.regex, OdtStylePropKeys.ColWidth.convert, required = false)
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

