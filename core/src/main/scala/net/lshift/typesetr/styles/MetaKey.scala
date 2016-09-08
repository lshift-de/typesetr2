package net.lshift.typesetr
package styles

/**
 * A key to a single meta info entry
 */
abstract class MetaKey {
  /**
   * Name of the meta information under which it can be identified
   * @return
   */
  def name: String

  /**
   * Raw name of the meta info key that is supposed to be used in the target document
   */
  def rawName: String

  /**
   * Can the value of the meta entry be comma separated, i.e. can it represent a list of values
   * @return
   */
  def isCommaSeparated: Boolean

  override def hashCode: Int = name.hashCode

  override def equals(o: Any): Boolean = o match {
    case m: MetaKey => name equals m.name
  }

}

object MetaKey {

  def apply(name: String): MetaKey = BasicMetaKey(name)

  private case class BasicMetaKey(name: String) extends MetaKey {

    def rawName: String = name

    def isCommaSeparated: Boolean = false

  }

  implicit class MetaKeyOps(val x: MetaKey) extends AnyVal {
    /**
     * A latex-compliant version of the meta key divides the original
     * name by dashes, and turns the individual parts into an upper case words
     *
     * @return a latex-compliant interpretation of the meta key
     */
    def latexify = {
      val presuffix: Option[(String, String)] = x.name.split('-').toList match {
        case head :: rest => Some((head, rest.map(_.toUpperCase).mkString("")))
        case _            => None
      }
      for {
        (pre, suffix) <- presuffix
      } yield (pre + suffix)
    }
  }
}
