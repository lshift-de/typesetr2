package net.lshift.typesetr
package pandoc

import net.lshift.typesetr.parsers.{Repr, NodeFactory, ImageKind}

import scala.xml.Text

object Markers {

  private[this] var counter = 0

  private[this] def increment(): Int = {
    val res = counter
    counter = counter + 1
    res
  }

  private def wrap[T](name: String, beginName: String, endName: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] = {
    val name1 = name + "-" + increment()
    (Repr.makeTextElem(s"$beginName!$name1!", synthetic = false) +:
      txt) :+
      Repr.makeTextElem(s"$endName!$name1!", synthetic = false)
  }

  private def env[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    wrap(name, BeginEnv, EndEnv)(txt)

  private def cmd[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    wrap(name, BeginCmd, EndCmd)(txt)

  private def verbatim[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    wrap(name, BeginFormat, EndFormat)(txt)

  def citationInBlockQuotation[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    cmd(RightAlign)(txt)

  def formatBlock[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    verbatim(Verbatim)(txt)

  private def imgBlock[T](imgKind: String, txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    wrap(imgKind, BeginImgEnv, EndImgEnv)(txt)

  def inlineImg[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    imgBlock(ImgInline, txt)

  def blockImg[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    imgBlock(ImgBlock, txt)

  def fullWidthImg[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    imgBlock(ImgFull, txt)

  def rawImg[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    imgBlock(ImgRaw, txt)

  def mathBlock(txt: => String): String = {
    val name1 = Math + "-" + increment()
    s"$BeginMath!$name1!$txt$EndMath!$name1!"
  }

  final val RightAlign = "attrib"
  final val Verbatim = "verbatim"
  final val Math = "math"
  final val ImgInline = "inline"
  final val ImgBlock = "block"
  final val ImgFull = "fullwidth"
  final val ImgRaw = "raw"

  private final val BeginEnv = "TYPESETRENVSTART"

  private final val EndEnv = "TYPESETRENVEND"

  private final val BeginCmd = "TYPESETRCMDSTART"

  private final val EndCmd = "TYPESETRCMDEND"

  private final val BeginFormat = "TYPESETRPRESTART"

  private final val EndFormat = "TYPESETRPREEND"

  private final val BeginMath = "TYPESETRMATHSTART"

  private final val EndMath = "TYPESETRMATHEND"

  private final val BeginImgEnv = "TYPESETRIMGSTART"

  private final val EndImgEnv = "TYPESETRIMGEND"

  final val groupStartName = "group-start"
  final val groupName = "content"
  final val groupEndName = "group-end"
  final val imgKindGroupName = "kind"

  final val EnvR = new scala.util.matching.Regex(s"(?s)$BeginEnv!(.*)-(\\d*)!(.*?)$EndEnv!(.*)-\\2!", groupStartName, "id", groupName, groupEndName)

  final val CmdR = new scala.util.matching.Regex(s"(?s)$BeginCmd!(.*)-(\\d*)!(.*?)$EndCmd!(.*)-\\2!", groupStartName, "id", groupName, groupEndName)

  final val PreR = new scala.util.matching.Regex(s"(?s)${BeginFormat}!$Verbatim-(\\d*)!(.*?)${EndFormat}!$Verbatim-\\1!", "id", groupName)

  final val MathR = new scala.util.matching.Regex(s"${BeginMath}!$Math-(\\d*)!(.*?)${EndMath}!$Math-\\1!", "id", groupName)

  final val ImgR = new scala.util.matching.Regex(s"(?s)${BeginImgEnv}!(.*)-(\\d*)!(.*?)${EndImgEnv}!\\1-\\2!", imgKindGroupName, "id", groupName)

}

/**
  * A typeclass encapsulating information on how the image node
  * should be formatted, in a pre-pandoc document.
  */
sealed abstract class ImageFormatting {
  type K <: ImageKind
  def format[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]]
}

object ImageFormatting {

  type Aux[S <: ImageKind] = ImageFormatting { type K = S }

  implicit def inlineImg: ImageFormatting.Aux[parsers.InlineImg.type] =
    new ImageFormatting {

      type K = parsers.InlineImg.type

      def format[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
        Markers.inlineImg(txt)(factory)

    }

  implicit def blockImg: ImageFormatting.Aux[parsers.BlockImg.type] =
    new ImageFormatting {

      type K = parsers.BlockImg.type

      def format[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
        Markers.blockImg(txt)(factory)

    }


  implicit def fullWidthImg: ImageFormatting.Aux[parsers.FullWidthImg.type] =
    new ImageFormatting {

      type K = parsers.FullWidthImg.type

      def format[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
        Markers.fullWidthImg(txt)(factory)

    }

  implicit def rawImg: ImageFormatting.Aux[parsers.RawImg.type] =
    new ImageFormatting {

      type K = parsers.RawImg.type

      def format[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
        Markers.rawImg(txt)(factory)

    }

}
