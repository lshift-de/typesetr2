package net.lshift.typesetr
package pandoc

import cmd.Config
import postprocessors.PandocPostProcessor

/**
  * Post-processing of the Pandoc output into the desired
  * final output format.
  */
abstract class Writer {

  /**
    * The type of the Pandoc output value for the given output format
    */
  type BodyTpe

  /**
    * The desired output format e.g., LaTex, Markdown, HTML.
    */
  type Out <: cmd.OutputFormat

  /**
   * Translate the document into the final format.
   * Upon the successful document translation the output is provided
   * at the location determined by the `config`.
   *
   * @param config (parsed) configuration provided to the converter
   * @param logger logging utilities
   */
  def write(config: Config)(implicit logger: util.Logger): Unit

  /**
   * Post-pandoc conversion fixes of the body of the document.
   *
   * Pandoc may not necessarily produce final document that agrees
   * with our template formatting. Therefore we can define additional
   * post-translation modification to the techniques.
   * Note that this method *should* not perform any additional parsing
   * but rather only focus on minor style changes.
   *
   * @param body value representing the body of the document
   * @param ppp pandoc post processor that can introduce typesetr
   *            specific envs and cmds in the target document format
   * @return the (potentially modified) body of the new document
   */
  protected def bodyFixes(body: BodyTpe)(implicit ppp: PandocPostProcessor.Aux[Out, BodyTpe], log: util.Logger): BodyTpe

}

object Writer {

  // Constant that marks a space introduced in the pre-formatted text by the
  // Typesetr's pre-processor.
  // Pandoc does not understand the concept of spaces in our code blocks
  // so we have to manually workaround those.
  final val TypesetrPreSpace = "!!"

}
