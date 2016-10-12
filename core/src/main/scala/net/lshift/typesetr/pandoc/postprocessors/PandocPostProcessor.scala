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
    * Replace the environment's starting/ending tag,
    * and the contents within into the target's equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        environment's starting tag
    *        replaced by the target's equivalent
    */
  def replaceEnvBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe

  /**
    * Replace the command's starting/ending tag, and the contents
    * within into the target's equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        commands replaced by the target's equivalent
    */
  def replaceCmdBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe

  /**
    * Replace all pre-formatted fragments with the target's
    * equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        pre-formatted fragments replaced by the
    *        target's equivalent
    */
  def replaceFormattedBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe

  /**
    * Replace all inline math fragments with the target's
    * equivalent
    *
    * @param body content to translate
    * @return `body` with all the occurrences of the
    *        inline math fragments replaced by the
    *        target's equivalent
    */
  def replaceInlineMath(body: BodyTpe)(implicit log: util.Logger): BodyTpe

  /**
    * Replace all img fragments with the target's
    * equivalent
    *
    * @param body image representation to translate
    * @return `body` with all the occurrences of the
    *        inline math fragments replaced by the
    *        target's equivalent
    */
  def replaceImgBlock(body: BodyTpe)(implicit log: util.Logger): BodyTpe

}

object PandocPostProcessor {
  type Aux[T <: cmd.OutputFormat, B] =
    PandocPostProcessor { type BodyTpe = B; type Out = T }
}


