package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

case class FontFamily(name: String)

object FontFamily {
  lazy val Arial = FontFamily("Arial")

  implicit def strToFont(name: String): FontFamily =
    FontFamily(name)
}
