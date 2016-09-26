package net.lshift.typesetr
package parsers

import util.Logger

import java.io.File

/**
 * Interface of the generic document parser
 */
abstract class Parser {

  // Base class representing a basic document node
  type DocNode

  /**
   * Parse a given file and return a typesetr's
   * interpretation of it.
   *
   * @param input input file to be parsed
   * @return interpretation of the document in
   *         typesetr's internal format
   */
  def parse(input: File,
            makeTransclusions: Boolean)(
              implicit logger: Logger, config: cmd.Config): ParsedDocument[DocNode]

  /**
   * Return document-specific functions for analyzing
   * document's nodes
   */
  implicit def nodeConfig: NodeConfigs.WithNode[DocNode]

}
