package net.lshift.typesetr
package parsers
package odt
package styles

import xml.attributes.StyleAttribute
import xml.{ XmlAttribute, Tag}
import util.Units

sealed abstract class StylePropKey { self =>

  // Type of the property's value
  type Result

  /*
    * The low-level xml attribute
    *
    * The attribute has a string value that needs to be
    * validated and translated into a first-class Scala object
    */
  def name: Option[XmlAttribute]

  type V = self.type

  implicit val Tag: V

}

object StylePropKey {

  type Aux[T] = StylePropKey { type Result = T }

  trait RegexConverter {
    self: StylePropKey =>

    def regex: scala.util.matching.Regex

    protected def toResult: String => self.Result

    def convert(x: String): Option[self.Result] =
      regex.findFirstMatchIn(x).map(x => toResult(x.matched))

    protected def firstGroup(x: String): Option[Units] =
      regex.findFirstMatchIn(x).flatMap(v => Units.parse((v.group(1).toInt, v.group(3))))

  }

  type Of = StylePropKey { type Result <: StyleAttribute }
  // TODO: Figure out why the type inferencer goes
  // nuts when we use With[StyleAttribute] instead
  type With[+T] = StylePropKey { type Result <: T }

  case object Tpe extends StylePropKey {
    type Result = StyleType
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = None
  }

  case object FontFamily extends StylePropKey {
    type Result = xml.attributes.FontFamily
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.StyleFFamilyName)
  }

  case object FontSeize extends StylePropKey with RegexConverter {
    type Result = Units
    val regex = "(\\d+)(.\\d+)?(pt|\\%)".r
    implicit val Tag: this.type = this
    def toResult = (x: String) => firstGroup(x).getOrElse(Units.default)
    def name: Option[XmlAttribute] = Some(OdtTags.FoFSize)
  }

  case object FontWeight extends StylePropKey {
    type Result = xml.attributes.FontWeight
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.FoFWeight)
  }

  case object FontStyleProp extends StylePropKey {
    type Result = xml.attributes.FontStyle
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.FoFStyle)
  }

  case object Underline extends StylePropKey {
    type Result = xml.attributes.Underline
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.StyleTextUnderline)
  }

  case object LineThrough extends StylePropKey {
    type Result = xml.attributes.LineThrough
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.StyleTextLinethrough)
  }

  case object Color extends StylePropKey {
    type Result = xml.attributes.Color
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.FoColor)
  }

  case object BackgroundColor extends StylePropKey {
    type Result = xml.attributes.Color
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.FoBColor)
  }

  case object TextPosition extends StylePropKey {
    type Result = xml.attributes.TextPosition
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.StyleTextPosition)
  }

  case object TextAlign extends StylePropKey {
    type Result = xml.attributes.TextAlign
    implicit val Tag: this.type = this
    def toResult: String => Option[xml.attributes.TextAlign] = {
      (x: String) => xml.attributes.TextAlign.stringToTextAlign(x) match {
        case Some(xml.attributes.TextAlign.Start) => Some(xml.attributes.TextAlign.Left)
        case Some(xml.attributes.TextAlign.End) => Some(xml.attributes.TextAlign.Right)
        case other                        => other
      }
    }
    def name: Option[XmlAttribute] = Some(OdtTags.FoTextAlign)
  }

  case object LineHeight extends StylePropKey with RegexConverter {
    type Result = Units
    val regex = "-?(\\d+)([.]\\d*)?(%)".r
    implicit val Tag: this.type = this
    def toResult = (x: String) => firstGroup(x).getOrElse(Units.default)
    def name: Option[XmlAttribute] = Some(OdtTags.FoLineHeight)
  }

  // TODO: we seem to have situations when left margin is defined in inches
  case object MarginLeft extends StylePropKey with RegexConverter {
    type Result = Units
    val regex = "-?(\\d+)([.]\\d+)?(cm|in)".r
    implicit val Tag: this.type = this
    def toResult = (x: String) => firstGroup(x).getOrElse(Units.default)
    def name: Option[XmlAttribute] = Some(OdtTags.FoMarginLeft)
  }

  case object TextIndent extends StylePropKey with RegexConverter {
    type Result = Units
    implicit val Tag: this.type = this
    val regex = "-?(\\d+)([.]\\d+)?(cm)".r
    def toResult = (x: String) => firstGroup(x).getOrElse(Units.default)
    def name: Option[XmlAttribute] = Some(OdtTags.FoTextIndent)
  }

  case object ParBreak extends StylePropKey {
    type Result = xml.attributes.ParBreak
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = Some(OdtTags.FoParBreak)
  }

  case object SubListStyles extends StylePropKey {
    type Result = String // TODO
    implicit val Tag: this.type = this
    def name: Option[XmlAttribute] = None
  }

  case object MinHeight extends StylePropKey with RegexConverter {
    type Result = Units
    val regex = "(\\d+)(.\\d+)?(cm)".r
    implicit val Tag: this.type = this
    def toResult = (x: String) => firstGroup(x).getOrElse(Units.default)
    def name: Option[XmlAttribute] = Some(OdtTags.StyleMinHeigh)
  }

  case object ColWidth extends StylePropKey with RegexConverter {
    type Result = Units
    val regex = "(\\d+)(.\\d+)?(cm)".r
    implicit val Tag: this.type = this
    def toResult = (x: String) => firstGroup(x).getOrElse(Units.default)
    def name: Option[XmlAttribute] = Some(OdtTags.StyleColumnWidth)
  }
}
