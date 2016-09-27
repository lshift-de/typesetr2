package net.lshift.typesetr
package parsers
package odt

import styles.OdtStyleId
import cmd.InputFormat
import net.lshift.typesetr.parsers.styles.StyleExtractor
import net.lshift.typesetr.styles.MetaFromDocument

/**
 * ODT-specific functions for analyzing/transforming the document
 */
class OdtNodeConfig extends NodeConfigs {

  type Format = InputFormat.Odt.type
  type Node = scala.xml.Node

  lazy val metaExtractor: MetaFromDocument =
    OdtMetaFromDocument.empty()

  lazy val styleExtractor: StyleExtractor.Aux[Node] =
    new StyleExtractor {
      type DocNode = Node
      def extractId(node: Repr.Aux[Node]): Option[parsers.styles.StyleId] =
        OdtStyleId.forNonStyleNode(node.source)
    }

  lazy val nodeFactory: NodeFactory.Aux[Node] = new OdtNodeFactory

  lazy val nodeInfo: NodeInfo.Aux[Node] = new OdtNodeInfo

}
