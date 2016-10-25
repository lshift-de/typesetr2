package net.lshift.typesetr
package parsers
package odt

import Repr._
import net.lshift.typesetr.xml.{ InternalTags, Attribute, Tag }

import scala.xml.{ PrefixedAttribute, TopScope, Elem, Text }

import scala.language.existentials

class OdtNodeFactory extends NodeFactory {

  import OdtTags.{ Text => TText, _ }

  type DocNode = scala.xml.Node

  def create(tag: Tag,
             elem: scala.xml.Node,
             children: Seq[Aux[scala.xml.Node]] = Nil,
             attrs: List[Attribute] = Nil,
             contents: Option[String] = None): Aux[scala.xml.Node] =
    OdtNodeRepr(elem, children, tag, contents, attrs)

  def copy(children: Seq[Aux[DocNode]], source1: DocNode)(repr: Aux[DocNode]) =
    OdtNodeRepr(source1, children, repr.tag, repr.contents, repr.attr)

  def imgWithCaption(img: Repr.Aux[DocNode], caption: Seq[Repr.Aux[DocNode]]): Repr.Aux[DocNode] = {
    /*
     <draw:frame>
       <draw:text-box>
         <text:p text:style-name="Caption">
         $img
         $caption
         </text:p>
       </draw:text-box>
     </draw:frame>
    */
    val imgSourceAttrs = img.source.attributes

    (for {
      width <- imgSourceAttrs.getTag(SvgWidth)
      height <- imgSourceAttrs.getTag(SvgHeight)
      pos <- imgSourceAttrs.getTag(AnchorTpe)
      styleName <- imgSourceAttrs.getTag(DrawStyleName)
    } yield {

      val img1Attrs = img.source.attributes.copyWith(AnchorTpe, "paragraph")
      val img1 = img.source.copy(meta = img1Attrs)
      val img1Repr = img.copy(source1 = img1)(this)

      val innerPAttr = new PrefixedAttribute(StyleNameAttr.namespace.short.value, StyleNameAttr.tag, "Caption", scala.xml.Null)
      val innerP =
        new Elem(prefix = OdtTags.P.namespace.short.value,
          label = OdtTags.P.tag,
          attributes1 = innerPAttr,
          scope = img.source.scope,
          minimizeEmpty = true,
          child = ((img1 +: caption.map(_.source)): _*))
      val innerPRepr =
        create(InternalTags.P, innerP, img1Repr +: caption)

      val textBoxAttr = new PrefixedAttribute(FoMinHeight.namespace.short.value, FoMinHeight.tag, height, scala.xml.Null)
      val textBox =
        new Elem(prefix = OdtTags.TextBox.namespace.short.value,
          label = OdtTags.TextBox.tag,
          attributes1 = textBoxAttr,
          scope = img.source.scope,
          minimizeEmpty = true,
          child = innerP)
      val textBoxRepr =
        create(Tag.nodeTag, textBox, innerPRepr :: Nil)

      val frameAttr =
        new PrefixedAttribute(DrawStyleName.namespace.short.value, DrawStyleName.tag, styleName,
          new PrefixedAttribute(AnchorTpe.namespace.short.value, AnchorTpe.tag, pos,
            scala.xml.Null))
      val frame =
        new Elem(prefix = OdtTags.Frame.namespace.short.value,
          label = OdtTags.Frame.tag,
          attributes1 = frameAttr,
          scope = img.source.scope,
          minimizeEmpty = true,
          child = textBox)
      create(InternalTags.FRAME, frame, textBoxRepr :: Nil)

    }).getOrElse(img)
  }

  def paragraphFrom(body: Seq[Repr.Aux[DocNode]], p0: Repr.Aux[DocNode]): Repr.Aux[DocNode] = {
    val p = p0.source.copy(body = body.map(_.source))
    p0.copy(children = body, source1 = p)(this)
  }

  def textNode(text: String): scala.xml.Node =
    Text(text)

  def newLineNode(): scala.xml.Node = {
    new Elem(
      prefix = OdtTags.Linebreak.namespace.short.value,
      label = OdtTags.Linebreak.tag,
      attributes1 = scala.xml.Null,
      scope = TopScope,
      minimizeEmpty = true)
  }

}
