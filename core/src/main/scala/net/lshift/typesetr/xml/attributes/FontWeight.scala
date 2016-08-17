package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class FontWeight(val v: String) extends StyleAttribute {

  def name: String = v

}

object FontWeight {

  implicit def stringToFontWeight(x: String): Option[FontWeight] = x match {
    case Normal.v => Some(Normal)
    case Bold.v   => Some(Bold)
    case _        => None
  }

  case object Normal extends FontWeight("normal")
  case object Bold extends FontWeight("bold")

}
