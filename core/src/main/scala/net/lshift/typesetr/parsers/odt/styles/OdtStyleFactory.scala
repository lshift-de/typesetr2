package net.lshift.typesetr
package parsers
package odt
package styles

import net.lshift.typesetr.parsers.styles.{ Style, StyleId, DocumentFormatingFactory }
import net.lshift.typesetr.util.{ Percentage, Inches, ValOfUnit }
import net.lshift.typesetr.xml.XmlAttribute
import net.lshift.typesetr.xml.attributes.{ TextAlign, FontStyle }

import scala.xml.{ TopScope, Elem, MetaData, Text }

import scala.language.implicitConversions

/**
 * Provides factories for creating ODT styles
 */
object OdtStyleFactory {

  /**
   * Create a style that mimics paragraph quoting
   * @param parent parent style reference
   * @param counter uuid
   * @return a new style name and a corresponding odt definition
   */
  def quotingStyle(parent: Style, counter: Int): (StyleId, DocumentFormatingFactory.Aux[scala.xml.Node]) = {
    val randomName = StyleId(parent.id.family, s"${parent.id.name}_${counter}")
    (randomName, QuotingStyleParagraph(parent, PandocQuoteLeftMargin))
  }

  /**
   * Create a style that formats the text so that it looks like an inlined code
   * @param parent parent style reference
   * @return a new style name and a corresponding odt definition
   */

  def inlineCodeStyle(parent: Style): (StyleId, DocumentFormatingFactory.Aux[scala.xml.Node]) = {
    val codeStyle = StyleId(Some("text"), InlineCodeStyleName)
    (codeStyle, CodeStyleText(parent))
  }

  /**
   * Create a style that mimics table's caption
   * @param parent parent style's name
   * @return a new style name and a corresponding odt definition
   */
  def tableCaptionStyle(parent: Style): (StyleId, DocumentFormatingFactory.Aux[scala.xml.Node]) = {
    val codeStyle = StyleId(Some(CaptionStyleNameAndFamily._2), CaptionStyleNameAndFamily._1)
    (codeStyle, TableCaptionParagraph("Standard"))
  }

  // Desired output along the lines of
  // <style:style style:name="Table" style:family="paragraph" style:parent-style-name="Standard" style:class="extra"/>
  private case class TableCaptionParagraph(parentStyleName: String) extends DocumentFormatingFactory {

    type DocNode = scala.xml.Node

    def create(styleId: StyleId)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val props = Seq.empty[Repr.Aux[scala.xml.Node]]

      val meta =
        List(
          (OdtTags.StyleName, styleId.name),
          (OdtTags.StyleFamily, "paragraph"),
          (OdtTags.StyleClass, "extra"),
          (OdtTags.StyleParentStyle, parentStyleName))

      basicReprNode(
        new Elem(
          prefix = OdtTags.StyleStyle.namespace.toString,
          label = OdtTags.StyleStyle.tag,
          attributes1 = (scala.xml.Null).fromTags(meta),
          TopScope,
          minimizeEmpty = false, (props.map(_.source)): _*),
        nodes = props)
    }

    def modifyBody(styleId: StyleId, children: Seq[Repr.Aux[scala.xml.Node]])(implicit factory: NodeFactory.Aux[scala.xml.Node]): Seq[Repr.Aux[scala.xml.Node]] = {
      children
    }

    private def basicReprNode[T](source: T, nodes: Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Repr.Aux[T] = {
      Repr.makeElem(xml.Tag.nodeTag, body = nodes, contents = None, attrs = Nil)(
        source, factory)
    }

  }

  // Desired output along the lines of
  // <style:style style:name="some-random-name" style:family="paragraph" style:parent-style-name="$style-parent">
  //   <style:paragraph-properties fo:margin-left="0.5in" fo:margin-right="0in" fo:line-height="115%" fo:text-align="justify" style:justify-single-word="false" fo:text-indent="0in" style:auto-text-indent="false" fo:break-before="auto" fo:break-after="auto" style:writing-mode="lr-tb"/>
  //   <style:text-properties fo:font-style="italic" style:font-style-asian="italic" style:font-style-complex="italic"/>
  // </style:style>
  private case class CodeStyleText(parentStyle: Style) extends DocumentFormatingFactory {

    type DocNode = scala.xml.Node

    def create(styleId: StyleId)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val props = Seq.empty[Repr.Aux[scala.xml.Node]]

      val meta =
        List(
          (OdtTags.StyleName, styleId.name),
          (OdtTags.StyleFamily, "text"),
          (OdtTags.StyleParentStyle, parentStyle.id.name))

      basicReprNode(
        new Elem(
          prefix = OdtTags.StyleStyle.namespace.toString,
          label = OdtTags.StyleStyle.tag,
          attributes1 = (scala.xml.Null).fromTags(meta),
          parentStyle.source.map(_.asInstanceOf[Elem].scope).getOrElse(TopScope),
          minimizeEmpty = false, (props.map(_.source)): _*),
        nodes = props)
    }

    def modifyBody(styleId: StyleId, children: Seq[Repr.Aux[scala.xml.Node]])(implicit factory: NodeFactory.Aux[scala.xml.Node]): Seq[Repr.Aux[scala.xml.Node]] = {
      children
    }

    private def basicReprNode[T](source: T, nodes: Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Repr.Aux[T] = {
      Repr.makeElem(xml.Tag.nodeTag, body = nodes, contents = None, attrs = Nil)(
        source, factory)
    }

  }

  // Desired output along the lines of
  // <style:style style:name="some-random-name" style:family="paragraph" style:parent-style-name="$style-parent">
  //   <style:paragraph-properties fo:margin-left="0.5in" fo:margin-right="0in" fo:line-height="115%" fo:text-align="justify" style:justify-single-word="false" fo:text-indent="0in" style:auto-text-indent="false" fo:break-before="auto" fo:break-after="auto" style:writing-mode="lr-tb"/>
  //   <style:text-properties fo:font-style="italic" style:font-style-asian="italic" style:font-style-complex="italic"/>
  // </style:style>
  private case class QuotingStyleParagraph(parentStyle: Style, indent: ValOfUnit) extends DocumentFormatingFactory {

    type DocNode = scala.xml.Node

    def create(styleId: StyleId)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val paragraphProps = paragraphWithLeftMargin(margin = indent, parentStyle.textAlign)
      val textProps = textPropertyWithFontStyle(FontStyle.Italic)
      val props = Seq(paragraphProps, textProps)

      val meta =
        List(
          (OdtTags.StyleName, styleId.name),
          (OdtTags.StyleFamily, "paragraph"),
          (OdtTags.StyleParentStyle, "Standard"))

      basicReprNode(
        new Elem(
          prefix = OdtTags.StyleStyle.namespace.toString,
          label = OdtTags.StyleStyle.tag,
          (scala.xml.Null).fromTags(meta), parentStyle.source.map(_.asInstanceOf[Elem].scope).getOrElse(TopScope),
          minimizeEmpty = false, (props.map(_.source)): _*),
        nodes = props)
    }

    def modifyBody(styleId: StyleId, children: Seq[Repr.Aux[scala.xml.Node]])(implicit factory: NodeFactory.Aux[scala.xml.Node]): Seq[Repr.Aux[scala.xml.Node]] = {
      children
    }

    private def paragraphWithLeftMargin(margin: ValOfUnit, textAlign: Option[TextAlign])(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val meta: List[(XmlAttribute, String)] =
        List(
          (OdtTags.FoMarginLeft, margin),
          (OdtTags.FoFStyle, FontStyle.Italic),
          (OdtTags.FoMarginRight, Inches(0)),
          (OdtTags.FoMarginTop, Inches(0)),
          (OdtTags.FoMarginBottom, Inches(0)),
          (OdtTags.FoLineHeight, Percentage(115)),
          (OdtTags.FoTextAlign, textAlign.map(_.name).getOrElse(TextAlign.Justify.name)),
          (OdtTags.StyleJustifySingleWord, "false"),
          (OdtTags.FoTextIndent, Inches(0)),
          (OdtTags.StyleAutoTextIndent, "false"),
          (OdtTags.StyleWritingMode, "lr-tb"))

      basicReprNode(new Elem(prefix = OdtTags.StylePProps.namespace.toString,
        label = OdtTags.StylePProps.tag,
        (scala.xml.Null).fromTags(meta), parentStyle.source.map(_.asInstanceOf[Elem].scope).getOrElse(TopScope),
        minimizeEmpty = false, Nil: _*), nodes = Nil)
    }

    private def textPropertyWithFontStyle[T](name: FontStyle)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val meta: List[(XmlAttribute, String)] = List(
        (OdtTags.FoFStyle, name),
        (OdtTags.StyleFStyleAsian, name),
        (OdtTags.StyleFStyleComplex, name))

      basicReprNode(new Elem(prefix = OdtTags.StylePProps.namespace.toString,
        label = OdtTags.StyleTProps.tag,
        (scala.xml.Null).fromTags(meta), parentStyle.source.map(_.asInstanceOf[Elem].scope).getOrElse(TopScope),
        minimizeEmpty = false, Nil: _*), nodes = Nil)
    }

    private def basicReprNode[T](source: T, nodes: Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Repr.Aux[T] = {
      Repr.makeElem(xml.Tag.nodeTag, body = nodes, contents = None, attrs = Nil)(
        source, factory)
    }

    implicit def entry[T](v: (XmlAttribute, T))(implicit conv: T => String): (XmlAttribute, String) =
      (v._1, conv(v._2))

  }

  private final val PandocQuoteLeftMargin = Inches(2)

  private implicit def entry[T](v: (XmlAttribute, T))(implicit conv: T => String): (XmlAttribute, String) =
    (v._1, conv(v._2))

  final val CaptionStyleNameAndFamily = ("Table", "paragraph")
  final val InlineCodeStyleName = "Source_Text"

}