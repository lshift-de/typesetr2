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

}

object NodeInfo {

  type Aux[T] = NodeInfo { type DocNode = T }

  implicit def fromConfig[T](implicit config: NodeConfigs.WithNode[T]): NodeInfo.Aux[T] =
    config.nodeInfo

}
