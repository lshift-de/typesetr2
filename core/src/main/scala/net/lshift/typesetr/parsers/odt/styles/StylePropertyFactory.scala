package net.lshift.typesetr
package parsers
package odt
package styles

import util.{ Inches, Units, Milimeters }

import scala.xml.{ TopScope, Elem, MetaData }

abstract class StylePropertyFactory[T] {
  def create(styleId: StyleId)(implicit factory: NodeFactory.Aux[T]): Repr.Aux[T]
}

object StylePropertyFactory {
  def odtQuoting(parent: Style): (StyleId, StylePropertyFactory[scala.xml.Node]) = {
    val randomName = StyleId(parent.id.family, parent.id.name + "_1")
    (randomName, QuotingStyleParagraph(parent, Inches(0.5)))
  }

  // Desired output along the lines of
  // <style:style style:name="some-random-name" style:family="paragraph" style:parent-style-name="$style-parent">
  //   <style:paragraph-properties fo:margin-left="0.5in" fo:margin-right="0in" fo:line-height="115%" fo:text-align="justify" style:justify-single-word="false" fo:text-indent="0in" style:auto-text-indent="false" fo:break-before="auto" fo:break-after="auto" style:writing-mode="lr-tb"/>
  //   <style:text-properties fo:font-style="italic" style:font-style-asian="italic" style:font-style-complex="italic"/>
  // </style:style>
  private case class QuotingStyleParagraph(parentStyle: Style, indent: Units) extends StylePropertyFactory[scala.xml.Node] {
    def create(styleId: StyleId)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val paragraphProps = paragraphWithLeftMargin(margin = indent)
      val textProps = textPropertyWithFontStyle("italic")
      val props = Seq(paragraphProps, textProps)

      val meta =
        List((OdtTags.StyleName, styleId.name),
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

    private def paragraphWithLeftMargin(margin: Units)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val meta =
        List(
          (OdtTags.FoMarginLeft, margin.toString),
          (OdtTags.FoMarginRight, "0in"),
          (OdtTags.FoLineHeight, "115%"),
          (OdtTags.FoTextAlign, "justify"),
          (OdtTags.StyleJustifySingleWord, "false"),
          (OdtTags.FoTextIndent, "0in"),
          (OdtTags.StyleAutoTextIndent, "false"),
          (OdtTags.FoParBreak, "auto"),
          (OdtTags.FoParBreakAfter, "auto"),
          (OdtTags.StyleWritingMode, "lr-tb"))

      basicReprNode(new Elem(prefix = OdtTags.StylePProps.namespace.toString,
        label = OdtTags.StylePProps.tag,
        (scala.xml.Null).fromTags(meta), parentStyle.source.map(_.asInstanceOf[Elem].scope).getOrElse(TopScope),
        minimizeEmpty = false, Nil: _*), nodes = Nil)
    }

    private def textPropertyWithFontStyle[T](name: String)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
      val meta = List(
        (OdtTags.FoFStyle, name),
        (OdtTags.StyleFStyleAsian, name),
        (OdtTags.StyleFStyleComplex, name))

      basicReprNode(new Elem(prefix = OdtTags.StylePProps.namespace.toString,
        label = OdtTags.StyleTProps.tag,
        (scala.xml.Null).fromTags(meta), parentStyle.source.map(_.asInstanceOf[Elem].scope).getOrElse(TopScope),
        minimizeEmpty = false, Nil: _*), nodes = Nil)
    }

    private def basicReprNode[T](source: T, nodes: Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Repr.Aux[T] = {
      Repr.makeElem(xml.Tag.nodeTag, body = nodes, contents = None)(
        source, factory)
    }

  }

}