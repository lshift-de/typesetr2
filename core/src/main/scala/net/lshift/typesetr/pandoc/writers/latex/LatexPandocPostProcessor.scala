package net.lshift.typesetr
package pandoc
package writers.latex

import postprocessors.PandocPostProcessor

class LatexPandocPostProcessor extends PandocPostProcessor {

  type BodyTpe = String

  type Out = cmd.OutputFormat.Tex.type

  def replaceEnvStart(body: BodyTpe): BodyTpe =
    Markers.BeginEnvR.replaceAllIn(body, "\n\n\\\\begin\\{$1\\}")

  def replaceCmdEnd(body: BodyTpe): BodyTpe =
    Markers.EndEnvR.replaceAllIn(body, "\\\\end\\{$1\\}")

  // TODO: will all commands need to be started in a separated
  //       line (hence \n\n)? Maybe we should have a second,
  //       non-newline tag?
  def replaceCmdStart(body: BodyTpe): BodyTpe =
    Markers.BeginCmdR.replaceAllIn(body, "\n\n\\\\$1\\{")

  def replaceEnvEnd(body: BodyTpe): BodyTpe =
    Markers.EndCmdR.replaceAllIn(body, "\\}")

}
