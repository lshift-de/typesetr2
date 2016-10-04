package net.lshift.typesetr
package pandoc
package writers.latex

import postprocessors.PandocPostProcessor

import scala.util.matching.Regex

class LatexPandocPostProcessor extends PandocPostProcessor {

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
            replaceAllLiterally("\\", "").                             // - remove backslashes introduced by Pandoc
            replaceAllLiterally("textbasckslash", "\\").               // - do an actual translation to \
            replaceAllLiterally("{}", "")) +                           // - workaround for Pandoc's bug that puts curly brackets for sub/sup-scripts in a wrong place
          Regex.quoteReplacement(s"\\)")                               //   e.g. x^{}2 instead of x^{2}
      })

}
