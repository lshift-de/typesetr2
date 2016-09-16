package net.lshift.typesetr
package pandoc.postprocessors

/**
  * A class that translates Typesetr's synthetic tags
  * into the appropriate environment and command tags in
  * the target document format.
  */
abstract class PandocPostProcessor {

  /**
    * The underlying type of the document
    */
  type BodyTpe

  /**
    * The target document format
    */
  type Out <: cmd.OutputFormat

  /**
    * Replace the environment's starting tag with the
    * target's equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        environment's starting tag
    *        replaced by the target's equivalent
    */
  def replaceEnvStart(body: BodyTpe): BodyTpe

  /**
    * Replace the environment's ending tag with the
    * target's equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        environment's starting tag
    *        replaced by the target's equivalent
    */
  def replaceEnvEnd(body: BodyTpe): BodyTpe

  /**
    * Replace the command's starting tag with the
    * target's equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        commands's starting tag
    *        replaced by the target's equivalent
    */
  def replaceCmdStart(body: BodyTpe): BodyTpe

  /**
    * Replace the commands's starting tag with the
    * target's equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        commands's starting tag
    *        replaced by the target's equivalent
    */
  def replaceCmdEnd(body: BodyTpe): BodyTpe

}

object PandocPostProcessor {
  type Aux[T <: cmd.OutputFormat, B] =
    PandocPostProcessor { type BodyTpe = B; type Out = T }
}


