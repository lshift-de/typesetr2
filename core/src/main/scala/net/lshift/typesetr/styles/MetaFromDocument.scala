package net.lshift.typesetr
package styles

/**
 * Meta information inferred from the body of the
 * document.
 *
 * Unlike the rest of meta data
 * (coming from metadata.yml or meta.xml)
 */
abstract class MetaFromDocument {

  def title: Option[MetaEntry]

  def subtitle: Option[MetaEntry]

  def withKey(key: String, value: String): MetaFromDocument

  def withTitle(t: String): MetaFromDocument

  def withSubTitle(t: String): MetaFromDocument

  def fromKey(key: MetaKey): Option[MetaEntry]

  def entries: List[(MetaKey, MetaEntry)]

  def isUpdateable: Boolean

}

object MetaFromDocument {

  final val title = MetaKey("title")

  final val subtitle = MetaKey("subtitle")

  lazy val keys: List[MetaKey] = List(title, subtitle)

}
