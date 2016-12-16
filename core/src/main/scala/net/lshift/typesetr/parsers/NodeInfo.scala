package net.lshift.typesetr
package parsers

/**
 * Typeclass for extracting/analyzing typesetr's internal
 * representation of the document.
 */
abstract class NodeInfo {

  type DocNode

  /**
   * Is the given node a text node i.e. no children nodes
   * involved.
   */
  def isText(node: Repr.Aux[DocNode]): Boolean

  /**
   * Infer the document's content node from the given node.
   * The `content node` means the one that has strictly the
   * whole text data (and no style, scripts etc)
   *
   * @param root a root node of the document
   */
  def docContent(root: Repr.Aux[DocNode]): List[Repr.Aux[DocNode]]

  /**
   * Does the current node represent the document's content?
   *
   * @param node node of the document
   * @param nestingLevel the level of nesting from the `root`
   * @return true if the node contains content's body, false otherwise.
   */
  def isContentInBody(node: Repr.Aux[DocNode], nestingLevel: Int): Boolean

  /**
   * Can one of the children of the `node` contain the content body?
   *
   * @param node node of the document
   * @param nestingLevel the level of nesting from the `root`
   * @return true if one of the children can potentially have content's body, false otherwise
   */
  def canContainContent(node: Repr.Aux[DocNode], nestingLevel: Int): Boolean

  /**
   * Text representation
   *
   * @param node node of the document
   * @return empty, if impossible to retrieve a string representation, non-empty string otherwise
   */
  def textRepresentation(node: Repr.Aux[DocNode]): Option[String]

  /**
   * Does the given node represent a formatted text fragment
   *
   * @param node node of the document
   * @return true is the node represents the formatted text, false otherwise
   */
  def isFormattedText(node: Repr.Aux[DocNode]): Boolean

}

object NodeInfo {

  type Aux[T] = NodeInfo { type DocNode = T }

  implicit def fromConfig[T](implicit config: NodeConfigs.WithNode[T]): NodeInfo.Aux[T] =
    config.nodeInfo

}
