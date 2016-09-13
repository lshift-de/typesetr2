package net.lshift.typesetr
package parsers
package odt

import styles.StyleId

/**
 * Type class that is capable of extracting a style's
 * identifier from a given document's node, if any.
 */
abstract class StyleExtractor {

  type DocNode

  /**
   * Extract style identifier from the node, if any
   *
   * @param node node which we want to analyze
   * @return a style identifier associated with the given document's node
   */
  def extractId(node: Repr.Aux[DocNode]): Option[StyleId]

}

object StyleExtractor {

  type Aux[T] = StyleExtractor { type DocNode = T }

}
