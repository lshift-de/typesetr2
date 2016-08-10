package net.lshift.typesetr
package parsers.odt

import net.lshift.typesetr.parsers.{ ReprEmptyBuilder, Repr }

abstract class DocumentStyle {

  type Underlying

  def header: Repr.Aux[Underlying]

  def footer: Repr.Aux[Underlying]

  def textWidth: Int

  def styles: Map[String, Style]

}

object DocumentStyle {

  type Aux[T] = DocumentStyle { type Underlying = T }

  def apply[T](header0: Repr.Aux[T],
               footer0: Repr.Aux[T],
               textWidth0: Int): DocumentStyle.Aux[T] =
    new DocumentStyle {

      type Underlying = T

      def textWidth: Int = textWidth0

      def header: Repr.Aux[this.Underlying] = header0

      def footer: Repr.Aux[this.Underlying] = footer0

      def styles: Map[String, Style] = Map.empty

    }

  def empty[T](implicit reprBuilder: ReprEmptyBuilder[T]): DocumentStyle.Aux[T] =
    new EmptyDocumentStyle(reprBuilder)

  class EmptyDocumentStyle[T](repr: ReprEmptyBuilder[T]) extends DocumentStyle {

    type Underlying = T

    def header: Repr.Aux[T] = repr.empty()

    def textWidth: Int = 0

    def footer: Repr.Aux[T] = repr.empty()

    def styles: Map[String, Style] = Map.empty

  }

}
