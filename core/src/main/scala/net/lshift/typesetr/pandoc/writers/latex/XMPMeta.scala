package net.lshift.typesetr
package pandoc
package writers
package latex

/**
 * A representation of the meta information in LaTeX
 */
abstract class XMPMeta extends styles.MetaKey {

  /**
   * Pretty-printed name of the meta information
   */
  def name: String

  /**
   * Raw, as defined in the LaTeX document, name of the meta information
   */
  def rawName: String

  /**
   * Fallback raw name to be used if `rawName` is not present in the document
   */
  def fallback: Option[String]

}

object XMPMeta {

  lazy val author = XMPMeta("author", list = true)
  lazy val contactemail = XMPMeta("contactemail", list = true)
  lazy val contactphone = XMPMeta("contactphone", list = true)
  lazy val contacturl = XMPMeta("contacturl", list = true)
  lazy val contactaddress = XMPMeta("contactaddress", list = true)

  lazy val lang = XMPMeta("lang")
  lazy val title = XMPMeta("title")
  lazy val description = XMPMeta("description", "abstract", "subject")
  lazy val keywords = XMPMeta("keywords", list = true)
  lazy val copyright = XMPMeta("copyright")

  def all() = List(lang, title, author, description, keywords, copyright)

  def apply(name: String, list: Boolean = false): XMPMeta = XMPMetaImpl(name, list)
  def apply(name: String, fallback: String, key: String): XMPMeta = XMPMetaWithFallback(name, key, Some(fallback))
}

case class XMPMetaImpl(name: String, list: Boolean) extends XMPMeta {

  def fallback: Option[String] = None

  def isCommaSeparated: Boolean = list

  def rawName = name

}

case class XMPMetaWithFallback(name: String, key: String, fallback: Option[String]) extends XMPMeta {

  def isCommaSeparated: Boolean = false

  def rawName = key

}
