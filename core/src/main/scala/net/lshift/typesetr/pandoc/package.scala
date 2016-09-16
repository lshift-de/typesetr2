package net.lshift.typesetr

import cmd.OutputFormat
import pandoc.writers.latex.LatexPandocPostProcessor
import pandoc.postprocessors.PandocPostProcessor

package object pandoc {

  implicit lazy val latexPostProcessor: PandocPostProcessor.Aux[OutputFormat.Tex.type, String] =
    new LatexPandocPostProcessor

}
