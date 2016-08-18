package net.lshift.typesetr
package parsers

import util.Logger

import java.io.File

abstract class Parser {

  type Underlying

  // TODO: temporary interface extracted from
  // the original typesetr converter

  def parseToRawBody(input: File,
                     rewrittenInput: Boolean,
                     makeTransclusions: Boolean)(
                       implicit logger: Logger): Repr.Aux[Underlying]

  def rewriteInput(meta: Any,
                   unaugmentedMeta: Any,
                   transclusions: Any,
                   asides: Any,
                   rewriteInfo: Any): Any

}
