package net.lshift.typesetr
package parsers

import cmd.InputFormat
import parsers.odt.StyleExtractor
import styles.MetaFromDocument

/**
  * Input-independent configuration that returns
  * format-specific typeclasses to analyze/create
  * nodes of a document.
  */
abstract class NodeConfigs {

  // Input format type
  type Format <: InputFormat

  // The type of the class that represents the basic
  // node of the document
  type Node

  def metaExtractor: MetaFromDocument

  def styleExtractor: StyleExtractor.Aux[Node]

  def nodeFactory: NodeFactory.Aux[Node]

  def nodeInfo: NodeInfo.Aux[Node]

}

object NodeConfigs {

  type WithNode[N] = NodeConfigs { type Node = N }
  type OfFormat[F <: InputFormat] = NodeConfigs { type Format = F }

  type Aux[N, F <: InputFormat] = NodeConfigs { type Node = N; type Format = F }

  def fromInputFormat[T <: InputFormat, N](in: T)(implicit nodeConfigs: NodeConfigs.OfFormat[T]): NodeConfigs =
    nodeConfigs

}
