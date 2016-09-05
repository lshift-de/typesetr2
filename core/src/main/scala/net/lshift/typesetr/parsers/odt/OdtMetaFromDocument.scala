package net.lshift.typesetr
package parsers.odt

import net.lshift.typesetr.styles.{ MetaKey, MetaEntry, MetaFromDocument }

case class OdtMetaFromDocument(
  title0: Option[String],
  subtitle0: Option[String])
  extends MetaFromDocument {

  def withTitle(t: String): MetaFromDocument = {
    assert(this.title.isEmpty)
    OdtMetaFromDocument(title0 = Some(t), subtitle0 = this.subtitle0)
  }

  def withSubTitle(t: String): MetaFromDocument = {
    assert(this.subtitle.isEmpty)
    OdtMetaFromDocument(title0 = this.title0, subtitle0 = Some(t))
  }

  def title: Option[MetaEntry] = fromKey(MetaFromDocument.title)

  def subtitle: Option[MetaEntry] = fromKey(MetaFromDocument.subtitle)

  def fromKey(key: MetaKey): Option[MetaEntry] =
    if (key.name == MetaFromDocument.title.name) title0 flatMap (MetaEntry.apply)
    else if (key.name == MetaFromDocument.subtitle.name) subtitle0 flatMap (MetaEntry.apply)
    else None

  def entries: List[(MetaKey, MetaEntry)] =
    for {
      key <- MetaFromDocument.keys
      v <- fromKey(key)
    } yield (key, v)

  def isUpdateable: Boolean = title.isEmpty || subtitle.isEmpty

}

object OdtMetaFromDocument {

  def empty(): MetaFromDocument = OdtMetaFromDocument(None, None)

}
