package net.lshift.typesetr.parsers

abstract class ReprNullFactory[T] {
  def empty(): Repr.Aux[T]
}