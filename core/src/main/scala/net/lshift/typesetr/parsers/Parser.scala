package net.lshift.typesetr.parsers

import java.io.File

abstract class Parser {
  // TODO: temporary interface extracted from
  // the original typesetr converter

  def parseToRawBody(input: File,
                     rewrittenInput: Boolean,
                     makeTransclusions: Boolean): Any

  def rewriteInput(meta: Any,
                   unaugmentedMeta: Any,
                   transclusions: Any,
                   asides: Any,
                   rewriteInfo: Any): Any
}
