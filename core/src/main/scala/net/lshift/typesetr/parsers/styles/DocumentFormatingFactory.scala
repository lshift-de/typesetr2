package net.lshift.typesetr.parsers
package styles

/**
 * Typeclass that encapsulates the creation of document nodes
 * that define some formatting for a given document fragment.
 */
abstract class DocumentFormatingFactory {

  /**
   * The underlying type of the initial document
   */
  type DocNode

  /**
   * Create a style node information in the format
   * required by the document.
   *
   * @param styleId a unique id of the style
   * @param factory a document-dependent node factory
   * @return a new style node
   */
  def create(styleId: StyleId)(implicit factory: NodeFactory.Aux[DocNode]): Repr.Aux[DocNode]

  /**
   * Returns a modified fragment
   * @param styleId id of the style describing the formatting
   * @param children fragment of the document to be formatted
   * @param factory a document-dependent node factory
   * @return
   */
  def modifyBody(styleId: StyleId, children: Seq[Repr.Aux[DocNode]])(implicit factory: NodeFactory.Aux[DocNode]): Seq[Repr.Aux[DocNode]]

}

object DocumentFormatingFactory {

  type Aux[T] = DocumentFormatingFactory { type DocNode = T }

}
