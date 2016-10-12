package net.lshift.typesetr.parsers

import net.lshift.typesetr.pandoc.ImageFormatting
import net.lshift.typesetr.util.ValOfUnit

sealed abstract class ImageKind { self =>
  def size(width: ValOfUnit): ValOfUnit
  def formatting: ImageFormatting.Aux[self.type]
}

object ImageKind {
  def apply(s: String): Option[ImageKind] = s match {
    case InlineImg.kind    => Some(InlineImg)
    case BlockImg.kind     => Some(BlockImg)
    case FullWidthImg.kind => Some(FullWidthImg)
    case RawImg.kind       => Some(RawImg)
    case _                 => None
  }
}

/**
 * Inline the image, on the left margin
 */
case object InlineImg extends ImageKind { self =>
  val kind: String = "inline"
  def size(width: ValOfUnit): ValOfUnit = width
  val formatting: ImageFormatting.Aux[self.type] = implicitly[ImageFormatting.Aux[self.type]]
}

/**
 * Place the image in the block
 */
case object BlockImg extends ImageKind { self =>
  val kind: String = "block"
  def size(width: ValOfUnit): ValOfUnit = width
  val formatting: ImageFormatting.Aux[self.type] = implicitly[ImageFormatting.Aux[self.type]]
}

/**
 * Full-width image
 */
case object FullWidthImg extends ImageKind { self =>
  val kind: String = "fullwidth"
  def size(width: ValOfUnit): ValOfUnit = width
  val formatting: ImageFormatting.Aux[self.type] = implicitly[ImageFormatting.Aux[self.type]]
}

/**
 * Unmodified, pandoc-formatted image
 */
case object RawImg extends ImageKind { self =>
  val kind: String = "raw"
  def size(width: ValOfUnit): ValOfUnit = width
  val formatting: ImageFormatting.Aux[self.type] = implicitly[ImageFormatting.Aux[self.type]]
}
