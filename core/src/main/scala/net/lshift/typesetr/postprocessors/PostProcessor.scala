package net.lshift.typesetr
package postprocessors

import net.lshift.typesetr.parsers.{ NodeConfigs, Repr, DocumentStyle }
import styles.MetaFromDocument

/**
 * An interface to typesetr's first phase of
 * document post processing.
 * @tparam T the underlying type of the representation
 *           of the document (xml node, markdown node etc)
 */
abstract class PostProcessor[T] {

  def optimize(node: Repr.Aux[T])(implicit logger: util.Logger): Repr.Aux[T]

  def extractMeta(node: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): MetaFromDocument

  implicit protected def nodeConfig: NodeConfigs.WithNode[T]

}
