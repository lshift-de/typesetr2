package net.lshift.typesetr
package util

import parsers.{ Repr, NodeRepr }

import scala.annotation.tailrec
import scala.xml.{ Attribute => SAttribute, _ }
import xml._

class NodeOps(val x: scala.xml.Node) extends AnyVal {

  def hasTag(tags: List[Tag]): Boolean = x match {
    case elem: Elem =>
      tags.contains(Tag(elem.label))
    case _ =>
      false
  }

  def hasTag(target: Tag): Boolean =
    hasTag(List(target))

  def hasAttribute(attrs: List[Attribute]): Boolean = x match {
    case elem: Elem =>
      @tailrec
      def checkAttribute(m: MetaData): Boolean =
        (m != null) && ( // Lord, have mercy.
          attrs.contains(Attribute(m.key)) || checkAttribute(m.next))
      checkAttribute(elem.attributes)
    case _ =>
      false
  }

  def hasAttribute(attr: Attribute): Boolean =
    hasAttribute(List(attr))

  def getAttribute(attrName: String): Option[Seq[Node]] = x match {
    case elem: Elem =>
      val meta = elem.attributes(attrName)
      if (meta == null) None
      else Some(meta)
    case _ =>
      None
  }

  def hasAttrWithVal(attrName: String, v: String): Boolean =
    getAttribute(attrName).map(_.exists(n => n match {
      case Text(data) if data == v => true
      case _                       => false
    })).getOrElse(false)

  def \!(tag: XmlTag): Seq[scala.xml.Node] = x.child.filter(node =>
    node.prefix == tag.namespace.v && node.label == tag.tag)

  def \!!(tag: XmlTag): Option[scala.xml.Node] = x.child.find(node =>
    // TODO: include prefix checking against the namespace
    //node.prefix == tag.namespace.v &&
    node.label == tag.tag)

  def \!!(path: XmlPath): Option[scala.xml.Node] = {
    def aux(path0: XmlPath, n: scala.xml.Node): Option[scala.xml.Node] = path0 match {
      case XmlPath(Some(prefix), tag) =>
        aux(prefix, n) flatMap (_ \!! tag)
      case XmlPath(None, tag) =>
        n \!! tag
    }
    aux(path, x)
  }

  def wrap(tag: Tag,
           body: Seq[Repr.Aux[scala.xml.Node]],
           attributes: List[Attribute] = Nil,
           contents: Option[String] = None)(implicit builder: NodeRepr[scala.xml.Node]): Repr.Aux[scala.xml.Node] =
    if (contents.nonEmpty) {
      assert(body.isEmpty)
      builder.createWithContents(tag, x, contents.get)
    } else if (attributes.isEmpty)
      builder.create(tag, x, body)
    else
      builder.createWithAttributes(tag, x, body, attributes)

  def wrapRec(tag: Tag)(implicit builder: NodeRepr[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
    val children0 = x.child.map(_.wrapRec(tag))
    x.wrap(tag, children0, Nil)
  }

  def isBlank: Boolean =
    x.child forall isBlank

  def isBlank(node: scala.xml.Node): Boolean = node match {
    case Text(data) => data.trim == ""
    case _          => node.child forall isBlank
  }

  def xmlTag(implicit ns: NameSpaces): XmlTag =
    (ns(x.prefix), x.label)

  def withAttribute(attr: Attribute, body: Seq[Repr.Aux[scala.xml.Node]]): Option[scala.xml.Node] = (x, attr.value) match {
    case (node: Elem, Some(v)) =>
      val meta = new UnprefixedAttribute(attr.key.key, v, null)
      Some(new Elem(node.prefix, node.label, meta, node.scope, node.minimizeEmpty, body.map(_.source): _*))
    case _ =>
      None
  }

}

class MetaDataOps(val x: scala.xml.MetaData) extends AnyVal {
  // Note: retrieval of attributes API is seriously broken in scala.xml
  // therefore we workaround the problem by mapping them to the attribute map
  // and checking their keys

  def getTag(tag: XmlTag): Option[String] =
    genericGetTag(s"${tag.namespace.short}:${tag.tag}")

  private def genericGetTag(entry: String): Option[String] = {
    x.asAttrMap.get(entry)

  }

}
