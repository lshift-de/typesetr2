package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

case class Color(rgh: String) extends StyleAttribute {

  def name: String = rgh

}

object Color {

  implicit def fromColorToString(x: String): Option[Color] =
    rgbR.findFirstIn(x).map(v => Color(v))

  final val rgbR = "#[\\da-f]{6}".r
}
