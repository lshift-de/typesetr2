package net.lshift.typesetr
package pandoc
package writers.latex

import postprocessors.PandocPostProcessor
import org.python.util.PythonInterpreter

import scala.util.matching.Regex

class LatexPandocPostProcessor extends PandocPostProcessor {

  type BodyTpe = String

  type Out = cmd.OutputFormat.Tex.type

  def replaceEnvBlock(body: BodyTpe): BodyTpe = {
    Markers.EnvR.replaceAllIn(body,
      { m =>
        Regex.quoteReplacement(s"\\begin{${m.group(Markers.groupStartName)}}") +
          Regex.quoteReplacement(m.group(Markers.groupName)) +
          Regex.quoteReplacement(s"\\end{${m.group(Markers.groupStartName)}})")
      })
  }

  def replaceCmdBlock(body: BodyTpe): BodyTpe =
    Markers.CmdR.replaceAllIn(body,
      { m =>
        Regex.quoteReplacement(s"\\${m.group(Markers.groupStartName)}{") +
          Regex.quoteReplacement(m.group(Markers.groupName)) +
          Regex.quoteReplacement(s"}")
      })

  def replaceFormattedBlock(body: BodyTpe): BodyTpe = {
    val act = quotedWorkaround(Markers.PreR, Markers.groupName,
      s => pygmentsFormatter(preformattedText(s))) _
    act(body)
  }

  private def quotedWorkaround(regex: scala.util.matching.Regex,
                               groupName: String,
                               act: BodyTpe => BodyTpe)(body: BodyTpe): BodyTpe = {
    regex.replaceAllIn(body,
      m => Regex.quoteReplacement(act(m.group(groupName))))
  }

  private val pygmentsFormatter =
    { (x: BodyTpe) =>
      val interpreter = new PythonInterpreter()
      val codeVar = "code"
      val resultVar = "result"
      interpreter.set(codeVar, x)
      interpreter.exec(pythonCode(codeVar, resultVar, "python", LatexFormatter))
      interpreter.get(resultVar, classOf[String])
    }

  // Pandoc introduces some weird formatting, since it does not know
  // that the text is supposed to be left as-is.
  // So we have to fix it.
  private def preformattedText(body: BodyTpe): BodyTpe = {
    VerbatimDecoding.foldLeft(body) {
      case (b, (from, to)) =>
        b.replaceAllLiterally(from, to)
    }
  }

  // TODO: This is probably far from the complete list
  //       of characters encoded by Pandoc.
  //       Will need to be adapted on a per-need basis.
  private val VerbatimDecoding = Map(
    "\\" -> "",
    "{[}" -> "[",
    "{]}" -> "]")

  private def pythonCode(codeVar: String, resultVar: String, langName: String, formatter: PygmentsFormatter) =
    s"""|from pygments import highlight
        |from pygments.lexers import get_lexer_by_name
        |from pygments.formatters import ${formatter}
        |
        |$resultVar = highlight($codeVar, get_lexer_by_name(\"$langName\"), ${formatter}())
     """.stripMargin

  sealed abstract class PygmentsFormatter
  case object LatexFormatter extends PygmentsFormatter
  case object HtmlFormatter extends PygmentsFormatter

}
