package net.lshift.typesetr
package parsers
package odt

import net.lshift.typesetr.util.ValOfUnit
import xml.attributes.FontFamily

object Utils extends FontUtils
  with FigureUtils

trait FigureUtils { self: Utils.type =>

  import FigureUtils._

  /**
   * Returns a string representation of the image kind i.e., is
   * it an inlined image, or a block image.
   *
   * @param inlineAttr should the image be inlined
   * @param relativeWidth a relative width of the image (w.r.t. the document's text widht)
   * @return 'inline' if the image should be inlined, 'block' otherwise
   */
  def shouldInline(inlineAttr: Boolean, relativeWidth: Double): String =
    if (inlineAttr && (relativeWidth < MAX_INLINE_WIDTH_RATIO))
      InlineImg.kind
    else
      BlockImg.kind

  /**
   * Return relative width of the image that is nicely formatted
   *
   * @param relativeWidth width of the image w.r.t. the document's width (a value between 0 and 1)
   * @return relative width formatted to represent the image's width percentage-wise
   */
  def inferWidthPercentage(relativeWidth: Double): String = {
    val (width_perc, _) = snap_width_percentage(relativeWidth * 100)
    "%.2f".format(width_perc)
  }

  /**
   * Tests if the (possibly inlined) image should be marked as a full-width one
   *
   * @param inline does the document mark the image as inlined
   * @param relativeWidth width of the image w.r.t. the document's width (a value between 0 and 1)
   * @return true if the image should be a full-width one, false otherwise
   */
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

  /**
   * A test if the given font-family represents a font for code fragments
   * @param ff the instance of the font
   * @return true if the font is used for code fragments, false otherwise
   */
  def isCodeFont(ff: FontFamily): Boolean =
    ff.name.toLowerCase().endsWith(" mono") ||
      codeFonts.contains(ff)

}

object FigureUtils {

  private final val SNAPS_PER_PAGE = 33

  /**
   * Maximal ration when an image can be considered to be inline
   * rather than full-width block image.
   */
  private final val MAX_INLINE_WIDTH_RATIO = .75

}

trait FontUtils { self: Utils.type =>

  /**
   * List of fonts that mark the document fragment as a code block
   */
  lazy val codeFonts =
    List[FontFamily](
      "Anonymous Pro", "Consolas", "Courier", "Courier New", "Envy Code R",
      "Fixed", "fixed", "GNU Unifont", "Inconsolata", "Inconsolata-g", "Lucida Console",
      "M+ 1m", "Menlo", "Monaco", "Monofur", "OCR-A", "OCR-B",
      "Pragmata Pro", "Source Code Pro", "Terminal", "Terminus")

}