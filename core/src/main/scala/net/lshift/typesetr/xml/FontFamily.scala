package net.lshift.typesetr
package xml

case class FontFamily(name: String)

object FontFamily {
  lazy val Arial = FontFamily("Arial")

  implicit def strToFont(name: String): FontFamily =
    FontFamily(name)
}
