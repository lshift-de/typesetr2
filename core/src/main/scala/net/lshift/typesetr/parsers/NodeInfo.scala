package net.lshift.typesetr
package parsers

abstract class NodeInfo {

  type DocNode

  def isText(node: Repr.Aux[DocNode]): Boolean

  def docContent(node: Repr.Aux[DocNode]): List[Repr.Aux[DocNode]]

}

object NodeInfo {

  type Aux[T] = NodeInfo { type DocNode = T }

  implicit def fromConfig[T](implicit config: NodeConfigs.WithNode[T]): NodeInfo.Aux[T] =
    config.nodeInfo

}
