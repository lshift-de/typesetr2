package net.lshift.typesetr
package postprocessors

import parsers.styles.DocumentStyle
import parsers.{ NodeConfigs, Repr }
import styles.MetaFromDocument

/**
 * An interface to typesetr's first phase of
 * document post processing.
 *
 * @tparam T the underlying type of the representation
 *           of the document (xml node, markdown node etc)
 */
abstract class PostProcessor[T] {

  def optimize(node: Repr.Aux[T])(implicit logger: util.Logger, sty: DocumentStyle.Aux[T]): Repr.Aux[T]

  def inferMeta(node: Repr.Aux[T])(implicit docStyle: DocumentStyle.Aux[T]): Either[String, (Repr.Aux[T], MetaFromDocument)]

  implicit protected def nodeConfig: NodeConfigs.WithNode[T]

}
