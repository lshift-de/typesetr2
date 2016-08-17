package net.lshift.typesetr
package parsers.odt

import styles.StyleId

import net.lshift.typesetr.parsers.odt.styles.Style
import net.lshift.typesetr.parsers.{ ReprEmptyBuilder, Repr }

abstract class DocumentStyle { self =>

  type Underlying

  def header: Repr.Aux[Underlying]

  def footer: Repr.Aux[Underlying]

  def textWidth: Int

  def style(id: StyleId): Option[Style] = styles.get(id)

  def +:(style: (StyleId, Style)): self.type =
    if (styles.contains(style._1)) self
    else {
      updateStyles(style)
      self
    }

  protected def styles: Map[StyleId, Style]

  protected def updateStyles(style: (StyleId, Style)): self.type

  override def toString: String = {
    s"""|Document Style:
        | Text width: ${textWidth}
        |${styles.values.map(_.toString).mkString("\n")}""".stripMargin
  }

}

object DocumentStyle {

  type Aux[T] = DocumentStyle { type Underlying = T }

  def apply[T](header0: Repr.Aux[T],
               footer0: Repr.Aux[T],
               textWidth0: Int): DocumentStyle.Aux[T] =
    new DocumentStyle { self =>

      type Underlying = T

      def textWidth: Int = textWidth0

      def header: Repr.Aux[this.Underlying] = header0

      def footer: Repr.Aux[this.Underlying] = footer0

      protected var styles: Map[StyleId, Style] = Map.empty

      protected def updateStyles(style: (StyleId, Style)): self.type = {
        if (!styles.contains(style._1))
          styles = styles + style
        self
      }

    }

  def empty[T](implicit reprBuilder: ReprEmptyBuilder[T]): DocumentStyle.Aux[T] =
    new EmptyDocumentStyle(reprBuilder)

  class EmptyDocumentStyle[T](repr: ReprEmptyBuilder[T]) extends DocumentStyle { self =>

    type Underlying = T

    def header: Repr.Aux[T] = repr.empty()

    def textWidth: Int = 0

    def footer: Repr.Aux[T] = repr.empty()

    protected var styles: Map[StyleId, Style] = Map.empty

    protected def updateStyles(style: (StyleId, Style)): self.type = {
      if (!styles.contains(style._1))
        styles = styles + style
      self
    }

  }

}
