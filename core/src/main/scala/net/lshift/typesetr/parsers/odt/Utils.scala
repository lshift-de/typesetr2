package net.lshift.typesetr
package parsers
package odt

import net.lshift.typesetr.util.ValOfUnit
import xml.attributes.FontFamily

object Utils extends FontUtils
  with FigureUtils

trait FigureUtils { self: Utils.type =>

  import FigureUtils._

  def shouldInline(inlineAttr: Boolean, relativeWidth: Double): String =
    if (inlineAttr && (relativeWidth < MAX_INLINE_WIDTH_RATIO))
      InlineImg.kind
    else
      BlockImg.kind

  def inferWidthPercentage(relativeWidth: Double): String = {
    val (width_perc, _) = snap_width_percentage(relativeWidth * 100)
    "%.2f".format(width_perc)
  }

  def isFullWidthImg(inline: Boolean, relativeWidth: Double): Boolean = {
    val (width_perc, xlarge) = snap_width_percentage(relativeWidth * 100)
    !inline && (width_perc >= 100) && xlarge
  }

  private def snap_width_percentage(widthPercentage: Double): (Double, Boolean) = {
    val snaps = Math.round(SNAPS_PER_PAGE * (widthPercentage / 100.0))
    val xlarge = snaps > (SNAPS_PER_PAGE + 1)
    val snaps1 = Math.min(snaps, SNAPS_PER_PAGE)
    val width = Math.round(100 * snaps / SNAPS_PER_PAGE)
    (width, xlarge)
  }

  def isCodeFont(ff: FontFamily): Boolean =
    ff.name.toLowerCase().endsWith(" mono") ||
      codeFonts.contains(ff)

  case class FigureInfo(src: String, href: String)
  object FigureInfo {
    def fromNode[T <: scala.xml.Node](x: T): FigureInfo = ???
  }
}

object FigureUtils {

  private final val SNAPS_PER_PAGE = 33

  private final val MAX_INLINE_WIDTH_RATIO = .75

}

trait FontUtils { self: Utils.type =>
  lazy val codeFonts =
    List[FontFamily](
      "Anonymous Pro", "Consolas", "Courier", "Courier New", "Envy Code R",
      "Fixed", "fixed", "GNU Unifont", "Inconsolata", "Inconsolata-g", "Lucida Console",
      "M+ 1m", "Menlo", "Monaco", "Monofur", "OCR-A", "OCR-B",
      "Pragmata Pro", "Source Code Pro", "Terminal", "Terminus")
}