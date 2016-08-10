package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class FontStyle(val name: String)

object FontStyle {
  case object Italic extends FontStyle("italic")
  case object Normal extends FontStyle("normal")

  implicit def stringToFontStyle(v: String): FontStyle = v match {
    case Italic.name => Italic
    case Normal.name => Normal
  }
}

