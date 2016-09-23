package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

case class FontFamily(name: String) extends StyleAttribute

object FontFamily {
  lazy val Arial = FontFamily("Arial")

  implicit class FontFamilyOps(val x: FontFamily) extends AnyVal {

    def isCodeFont: Boolean = x.name.toLowerCase.endsWith(" mono") ||
      FontFamily.codeFonts.contains(x)

  }

  implicit def strToFont(name: String): FontFamily =
    FontFamily(name)

  private lazy val codeFonts =
    List[FontFamily](
      "Anonymous Pro", "Consolas", "Courier", "Courier New", "Envy Code R",
      "Fixed", "fixed", "GNU Unifont", "Inconsolata", "Inconsolata-g", "Lucida Console",
      "M+ 1m", "Menlo", "Monaco", "Monofur", "OCR-A", "OCR-B",
      "Pragmata Pro", "Source Code Pro", "Terminal", "Terminus")

}

