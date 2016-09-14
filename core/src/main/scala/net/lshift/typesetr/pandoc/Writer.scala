package net.lshift.typesetr
package pandoc

import cmd.Config

abstract class Writer {

  type BodyTpe

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
   * @return the (potentially modified) body of the new document
   */
  def bodyFixes(body: BodyTpe): BodyTpe

}
