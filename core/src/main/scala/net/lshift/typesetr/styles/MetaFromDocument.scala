package net.lshift.typesetr
package styles

/**
 * Meta information inferred from the body of the
 * document.
 *
 * This representation from the loaded meta info
 * (coming from metadata.yml or meta.xml)
 * which provides a `schema` for meta-entries.
 */
abstract class MetaFromDocument {

  def title: Option[String]

  def subtitle: Option[String]

  def withKey(key: String, value: String): MetaFromDocument

  def withTitle(t: String): MetaFromDocument

  def withSubTitle(t: String): MetaFromDocument

  def fromKey(key: MetaKey): Option[String]

  def entries: List[(MetaKey, String)]

  def isUpdateable: Boolean

}

object MetaFromDocument {

  final val title = MetaKey("title")

  final val subtitle = MetaKey("subtitle")

  lazy val keys: List[MetaKey] = List(title, subtitle)

}
