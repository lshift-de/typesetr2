package net.lshift.typesetr.parsers

abstract class PrettyPrinter[T] {

  def print(node: Repr.Aux[T]): Unit

}
