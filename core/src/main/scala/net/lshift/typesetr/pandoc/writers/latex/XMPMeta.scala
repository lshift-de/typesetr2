package net.lshift.typesetr
package pandoc
package writers
package latex

abstract class XMPMeta extends styles.MetaKey {

  def name: String

  def rawName: String

  def fallback: Option[String]

  def isCommaSeparated: Boolean

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

  def all() = List(lang, title, description, keywords, copyright)

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
