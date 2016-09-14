package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class FontStyle(val name: String) extends StyleAttribute

object FontStyle {

  case object Italic extends FontStyle("italic")
  case object Normal extends FontStyle("normal")

  implicit def stringToFontStyle(v: String): Option[FontStyle] = v match {
    case Italic.name => Some(Italic)
    case Normal.name => Some(Normal)
    case _           => None
  }

  implicit def toStringVal(x: FontStyle): String = x.name

}

