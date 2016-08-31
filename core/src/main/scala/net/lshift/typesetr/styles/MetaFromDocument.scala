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

  def title: Option[String]

  def subtitle: Option[String]

  def withTitle(t: String): MetaFromDocument

  def withSubTitle(t: String): MetaFromDocument

}
