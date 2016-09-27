package net.lshift.typesetr
package parsers
package odt
package styles

import net.lshift.typesetr.pandoc.Markers
import net.lshift.typesetr.parsers.styles.{ Style, StyleId, StylePropertyFactory }
import net.lshift.typesetr.util.{ Percentage, Inches, ValOfUnit }
import net.lshift.typesetr.xml.XmlAttribute
import net.lshift.typesetr.xml.attributes.{ TextAlign, FontStyle }

import scala.xml.{ TopScope, Elem, MetaData, Text }

import scala.language.implicitConversions

object OdtStylePropertyFactory {

  def odtQuoting(parent: Style): (StyleId, StylePropertyFactory[scala.xml.Node]) = {
    val randomName = StyleId(parent.id.family, parent.id.name + "_1")
    (randomName, QuotingStyleParagraph(parent, PandocQuoteLeftMargin))
  }

  // Desired output along the lines of
  // <style:style style:name="some-random-name" style:family="paragraph" style:parent-style-name="$style-parent">
  //   <style:paragraph-properties fo:margin-left="0.5in" fo:margin-right="0in" fo:line-height="115%" fo:text-align="justify" style:justify-single-word="false" fo:text-indent="0in" style:auto-text-indent="false" fo:break-before="auto" fo:break-after="auto" style:writing-mode="lr-tb"/>
  //   <style:text-properties fo:font-style="italic" style:font-style-asian="italic" style:font-style-complex="italic"/>
  // </style:style>
  private case class QuotingStyleParagraph(parentStyle: Style, indent: ValOfUnit) extends StylePropertyFactory[scala.xml.Node] {

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
          // FIXME: get rid of casting
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

}