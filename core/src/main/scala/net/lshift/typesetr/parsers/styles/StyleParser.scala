package net.lshift.typesetr.parsers.styles

import net.lshift.typesetr.util.Logger

abstract class StyleParser {

  /**
   * The underlying representation in the document used
   * for defining styles' meta information
   */
  type DocNode

  /**
   * Loads all the additional styles from the ODT's
   * content document (rather than from a meta document)
   *
   * @param rootStyle xml node of the styles aggregator
   * @param doc an initial representation of the document's styles
   * @return an updated document's style information
   */
  def loadFromDocContent(rootStyle: DocNode,
                         doc: DocumentStyle.Aux[DocNode])(implicit logger: Logger): DocumentStyle.Aux[DocNode]

  /**
   * Load style information from the node and append it to the existing
   * style dictionary.
   *
   * @param node the underlying node to be analyzed
   * @param doc the existing style dictionary
   * @param logger typesetr's logging utility
   * @return a (potentially updated) document's style dictionary
   */
  def appendStyleNode(node: DocNode,
                      doc: DocumentStyle.Aux[DocNode])(implicit logger: Logger): DocumentStyle.Aux[DocNode]

  /**
   * Parse a single style node and return
   * a validated and translated representation of it.
   *
   * @param node xml node representing a style property info
   * @param doc document's style information
   * @return a validated representation of the style property, if any
   */
  protected def parseStyle(node: DocNode,
                           doc: DocumentStyle)(implicit logger: Logger): Option[Style]

}

object StyleParser {

  type Aux[T] = StyleParser { type DocNode = T }

}
