package net.lshift.typesetr
package pandoc
package writers.latex

import postprocessors.PandocPostProcessor

import scala.util.matching.Regex

class LatexPandocPostProcessor extends PandocPostProcessor {
  import LatexPandocPostProcessor._

  type BodyTpe = String

  type Out = cmd.OutputFormat.Tex.type

  def replaceEnvBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe =
    Markers.EnvR.replaceAllIn(body,
      { m =>
        Regex.quoteReplacement(s"\\begin{${m.group(Markers.groupStartName)}}") +
          Regex.quoteReplacement(m.group(Markers.groupName)) +
          Regex.quoteReplacement(s"\\end{${m.group(Markers.groupStartName)}})")
      })

  def replaceCmdBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe =
    Markers.CmdR.replaceAllIn(body,
      { m =>
        Regex.quoteReplacement(s"\\${m.group(Markers.groupStartName)}{") +
          Regex.quoteReplacement(m.group(Markers.groupName)) +
          Regex.quoteReplacement(s"}")
      })

  def replaceFormattedBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe =
    Markers.PreR.replaceAllIn(body,
      m => Regex.quoteReplacement(PygmentsFormatter(m.group(Markers.groupName),
        LatexFormatter)))

  def replaceInlineMath(body: BodyTpe)(implicit log: util.Logger): BodyTpe =
    Markers.MathR.replaceAllIn(body,
      { m =>
        Regex.quoteReplacement(s"\\(") +
          Regex.quoteReplacement(m.group(Markers.groupName).
            replaceAllLiterally("\\textbackslash{}", "textbackslash"). // - replace all valid, encoded backslashes with a dummy
            replaceAllLiterally("\\", ""). // - remove backslashes introduced by Pandoc
            replaceAllLiterally("textbackslash", "\\"). // - do an actual translation to \
            replaceAllLiterally("{}", "")) + // - workaround for Pandoc's bug that puts curly brackets for sub/sup-scripts in a wrong place
          Regex.quoteReplacement(s"\\)") //   e.g. x^{}2 instead of x^{2}
      })

  def replaceImgBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe = {
    val keyvalentry: String => Option[(String, String)] = (s: String) => s.split("=").toList match {
      case k :: v :: Nil => Some((k, v))
      case _             => None
    }
    Markers.ImgR.replaceAllIn(body, { m =>
      val imgCommand = m.group(Markers.groupName).trim()
      if (imgCommand == "") ""
      else {
        val r = new Regex(s"(.*)\\\\${imgLatexCmd}\\[(.+)\\]\\{(.*)\\}.*", "prefix", "opts", "url")
        Regex.quoteReplacement(r.replaceAllIn(imgCommand, { mInner =>
          val prefix = mInner.group("prefix")
          m.group(Markers.imgKindGroupName) match {
            case Markers.ImgInline =>
              Regex.quoteReplacement(s"$prefix\\tystrmarginfigure{${mInner.group("url")}}")
            case Markers.ImgRaw =>
              Regex.quoteReplacement(s"${imgCommand}")
            case _ =>
              val opts = mInner.group("opts").split(",").toList.flatMap(s => keyvalentry(s)).toMap
              val width = opts.get("width").map(_.stripSuffix("\\textwidth")).getOrElse("1")
              val caption = "TODO" // TODO
              Regex.quoteReplacement(
                s"""|$prefix\\begin{adjustbox}{
                    |width=$width\\tystrfigurewidth,
                    |max totalsize={\\linewidth}{0.9\\textheight},center}
                    |\\includegraphics{${mInner.group("url")}}
                    |\\end{adjustbox}""".stripMargin)
            //Regex.quoteReplacement(s"$prefix\\tystrblockfigurenocap{$width}{${mInner.group("url")}}")

          }
        }))
      }

    })
  }

}

object LatexPandocPostProcessor {
  val imgLatexCmd = "includegraphics"
}
