package net.lshift.typesetr.parsers

import net.lshift.typesetr.xml.attributes.FontFamily

object Utils extends FontUtils
  with FigureUtils

trait FigureUtils { self: Utils.type =>
  def makeFigure[T](inline: Boolean,
                    relativeWidth: Int,
                    body: Seq[Repr.Aux[T]],
                    info: FigureInfo
                   ): Repr.Aux[T] =
    ???


  def isCodeFont(ff: FontFamily): Boolean =
    ff.name.toLowerCase().endsWith(" mono") ||
      codeFonts.contains(ff)

  case class FigureInfo(src: String, href: String)
  object FigureInfo {
    def fromNode[T <: scala.xml.Node](x: T): FigureInfo = ???
  }
}


trait FontUtils { self: Utils.type =>
  lazy val codeFonts =
    List[FontFamily](
      "Anonymous Pro", "Consolas", "Courier", "Courier New", "Envy Code R",
      "Fixed", "fixed", "GNU Unifont", "Inconsolata", "Inconsolata-g", "Lucida Console",
      "M+ 1m", "Menlo", "Monaco", "Monofur", "OCR-A", "OCR-B",
      "Pragmata Pro", "Source Code Pro", "Terminal", "Terminus")
}