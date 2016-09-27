package net.lshift.typesetr
package parsers

import styles.DocumentStyle

class ParsedDocument[T](val root: Repr.Aux[T],
                        val style: DocumentStyle.Aux[T])

object ParsedDocument {
  def apply[T](root: Repr.Aux[T],
               style: DocumentStyle.Aux[T]): ParsedDocument[T] =
    new ParsedDocument[T](root, style)
}
