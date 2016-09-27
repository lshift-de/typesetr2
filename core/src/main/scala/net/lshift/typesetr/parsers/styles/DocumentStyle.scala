package net.lshift.typesetr.parsers.styles

import net.lshift.typesetr.parsers.{ NodeInfo, Repr, ReprNullFactory }
import net.lshift.typesetr.util

import scalaz.Scalaz._
import scalaz.Tags.First

/**
 * Class representing the generic style information of the document.
 *
 * Stores a generic information about the header, footer of the document.
 * As well as a complete list of styles available/used in the original
 * document.
 */
abstract class DocumentStyle { self =>

  type Node

  /*
   * Header and footer nodes are (for some reason?) parsed
   * in the original Typesetr.
   * TODO: figure out why on earth do we need those?
   */
  def header: Repr.Aux[Node]

  def footer: Repr.Aux[Node]

  def textWidth: Int

  def listLevelDepth: Int

  def newListLevelContext: DocumentStyle.Aux[Node]

  /**
   * Return style information for the given id, if available.
   *
   * Style is uniquely identified by a family and name, but
   * not surprisingly most odt files only use name in text nodes,
   * so we also have a fallback mechanism for the latter.
   *
   * @param id id of the style to identify
   */
  def style(id: StyleId): Option[Style]

  /**
   * Record a style information under the given id
   *
   * @param style a tuple representing style's id and meta info
   */
  def +:(style: (StyleId, Style))(implicit logger: util.Logger): self.type = {
    if (styles.contains(style._1))
      logger.info((s"Overriding style ${style._1}"))
    //assert(!styles.contains(style._1), s"overriding style ${style._1}")
    updateStyles(style)
    self
  }

  /**
   * Infer a style that is applied to the given document node
   * Since we are generic, we implicitly provide a generic style info
   * extractor that is capable of extracting that from a generic document's node.
   *
   * @param node a node to which a style is being applied to
   * @param extr a generic style extractor for a document's node
   * @return a style that is being applied to the given node, if any
   */
  def styleForNode(node: Repr.Aux[Node])(implicit extr: StyleExtractor.Aux[Node], info: NodeInfo.Aux[Node]): Option[Style] = {
    if (implicitly[NodeInfo.Aux[Node]].isText(node)) None
    else {
      val styleIdOpt = extr.extractId(node)
      for {
        styleId <- styleIdOpt
        style <- this.style(styleId)
      } yield style
    }
  }

  protected def styles: Map[StyleId, Style]

  protected def updateStyles(style: (StyleId, Style)): self.type

  override def toString: String = {
    s"""|Document Style:
        | Text width: ${textWidth}
        |${styles.toList.sortBy(_._1).map(_._2.toString).mkString("\n")}""".stripMargin
  }

}

object DocumentStyle {

  type Aux[T] = DocumentStyle { type Node = T }

  def apply[T](header0: Repr.Aux[T],
               footer0: Repr.Aux[T],
               textWidth0: Int): DocumentStyle.Aux[T] =
    new DocumentStyle { self =>

      type Node = T

      def textWidth: Int = textWidth0

      def listLevelDepth: Int = 0

      def newListLevelContext: DocumentStyle.Aux[Node] =
        new ListDocumentStyle[T](self)

      def header: Repr.Aux[this.Node] = header0

      def footer: Repr.Aux[this.Node] = footer0

      protected var styles: Map[StyleId, Style] = Map.empty

      def style(id: StyleId): Option[Style] =
        scalaz.Tag.unwrap(First(styles.get(id)) |+| First(byNameFallback(id)))

      private def byNameFallback(id: StyleId): Option[Style] =
        styles.find(_._1.name == id.name).map(_._2)

      protected def updateStyles(style: (StyleId, Style)): self.type = {
        //if (!styles.contains(style._1))
        styles = styles + style
        self
      }

    }

  def empty[T](implicit reprBuilder: ReprNullFactory[T]): DocumentStyle.Aux[T] =
    new EmptyDocumentStyle(reprBuilder)

  private class ListDocumentStyle[T](prev: DocumentStyle.Aux[T]) extends DocumentStyle { self =>
    type Node = T

    def header: Repr.Aux[T] = prev.header

    def newListLevelContext: Aux[T] = new ListDocumentStyle[T](self)

    protected def styles: Map[StyleId, Style] = prev.styles

    def textWidth: Int = prev.textWidth

    def footer: Repr.Aux[T] = prev.footer

    protected def updateStyles(style: (StyleId, Style)): ListDocumentStyle.this.type = ???

    def listLevelDepth: Int = prev.listLevelDepth + 1

    def style(id: StyleId): Option[Style] = prev.style(id)
  }

  private class EmptyDocumentStyle[T](repr: ReprNullFactory[T]) extends DocumentStyle { self =>

    type Node = T

    def textWidth: Int = 0

    def header: Repr.Aux[T] = repr.empty()

    def listLevelDepth: Int = 0

    def newListLevelContext: Aux[T] = new ListDocumentStyle[T](self)

    def footer: Repr.Aux[T] = repr.empty()

    def style(id: StyleId): Option[Style] = None

    protected var styles: Map[StyleId, Style] = Map.empty

    protected def updateStyles(style: (StyleId, Style)): self.type = {
      //if (!styles.contains(style._1))
      styles = styles + style
      self
    }

  }

}
