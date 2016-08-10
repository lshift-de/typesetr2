package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class FontWeight(val v: String)

object FontWeight {

  implicit def stringToFontWeight(x: String): FontWeight = x match {
    case Normal.v => Normal
    case Bold.v   => Bold
  }

  case object Normal extends FontWeight("normal")
  case object Bold extends FontWeight("bold")

}
