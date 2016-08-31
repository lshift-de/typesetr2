package net.lshift.typesetr
package parsers.odt

import net.lshift.typesetr.styles.MetaFromDocument

case class OdtMetaFromDocument(
  title: Option[String],
  subtitle: Option[String])
  extends MetaFromDocument {

  def withTitle(t: String): MetaFromDocument = {
    assert(this.title.isEmpty)
    OdtMetaFromDocument(title = Some(t), subtitle = this.subtitle)
  }

  def withSubTitle(t: String): MetaFromDocument = {
    assert(this.subtitle.isEmpty)
    OdtMetaFromDocument(title = this.title, subtitle = Some(t))
  }

}

object OdtMetaFromDocument {

  def empty(): MetaFromDocument = OdtMetaFromDocument(None, None)

}
