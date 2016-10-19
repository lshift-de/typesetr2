package net.lshift.typesetr
package parsers

import styles.DocumentStyle

/**
 * Wrapper for the internal representation of the document.
 *
 * @param root root element of the document
 * @param style style dictionary inferred from the original document
 * @tparam T the underlying type of the node in the original document
 */
case class ParsedDocument[T](root: Repr.Aux[T],
                             style: DocumentStyle.Aux[T])
