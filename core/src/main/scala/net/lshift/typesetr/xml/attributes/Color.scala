package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions
case class Color(rgh: String)

object Color {
  implicit def fromColorToString(x: String): Color = Color(x)

  final val rgbR = "#[\\da-f]{6}".r
}
