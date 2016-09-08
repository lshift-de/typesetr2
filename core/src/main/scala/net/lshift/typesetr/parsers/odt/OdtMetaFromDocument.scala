package net.lshift.typesetr
package parsers.odt

import net.lshift.typesetr.styles.{ MetaKey, MetaEntry, MetaFromDocument }

case class OdtMetaFromDocument(
  title0: Option[String],
  subtitle0: Option[String])(mapKeys: Map[MetaKey, String])
  extends MetaFromDocument {

  def withKey(key: String, value: String): MetaFromDocument = {
    val metaKey = MetaKey(key)
    if (!mapKeys.contains(metaKey))
      OdtMetaFromDocument(title0 = this.title0, subtitle0 = this.subtitle0)(mapKeys + (metaKey -> value))
    else this
  }

  def withTitle(t: String): MetaFromDocument = {
    //assert(this.title.isEmpty, s"An attempt to add title '$t' given the existing title $title0")
    if (this.title.isEmpty)
      OdtMetaFromDocument(title0 = Some(t), subtitle0 = this.subtitle0)(mapKeys)
    else
      this
  }

  def withSubTitle(t: String): MetaFromDocument = {
    //assert(this.subtitle.isEmpty)
    if (this.subtitle.isEmpty)
      OdtMetaFromDocument(title0 = this.title0, subtitle0 = Some(t))(mapKeys)
    else
      this
  }

  def title: Option[String] = fromKey(MetaFromDocument.title)

  def subtitle: Option[String] = fromKey(MetaFromDocument.subtitle)

  def fromKey(key: MetaKey): Option[String] =
    if (key.name == MetaFromDocument.title.name) title0
    else if (key.name == MetaFromDocument.subtitle.name) subtitle0
    else mapKeys.get(key)

  def entries: List[(MetaKey, String)] =
    for {
      key <- MetaFromDocument.keys ++ mapKeys.keys
      v <- fromKey(key)
    } yield (key, v)

  def isUpdateable: Boolean = title.isEmpty || subtitle.isEmpty

  override def toString: String =
    s"""|MetaFromDocument:
        |${super.toString}
        |${mapKeys.toString}""".stripMargin

}

object OdtMetaFromDocument {

  def empty(): MetaFromDocument = OdtMetaFromDocument(None, None)(Map.empty)

}
