package net.lshift.typesetr
package util

import parsers.{ Repr, NodeFactory }

import scala.annotation.tailrec
import scala.xml.{ Attribute => SAttribute, _ }
import xml._

/**
 * Extension methods for Scala's xml node
 */
class NodeOps(val x: scala.xml.Node) extends AnyVal {

  def hasTag(tags: List[Tag]): Boolean = x match {
    case elem: Elem =>
      tags.contains(Tag(elem.label))
    case _ =>
      false
  }

  def hasTag(target: Tag): Boolean =
    hasTag(target :: Nil)

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
    hasAttribute(attr :: Nil)

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
           contents: Option[String] = None)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] =
    if (contents.nonEmpty) {
      assert(body.isEmpty)
      factory.createWithContents(tag, x, contents.get)
    } else
      factory.create(tag, x, body, attrs = attributes)

  def wrapRec(tag: Tag)(implicit factory: NodeFactory.Aux[scala.xml.Node]): Repr.Aux[scala.xml.Node] = {
    val children0 = x.child.map(_.wrapRec(tag))
    x.wrap(tag, children0, Nil)
  }

  def isBlank: Boolean =
    x.child forall {
      case Text(data) => data.trim == ""
      case node       => node.child forall { n => n.isBlank }
    }

  def xmlTag(implicit ns: NameSpaces): XmlTag =
    (ns(x.prefix), x.label)

  def withAttribute(attr: Attribute, body: Seq[Repr.Aux[scala.xml.Node]]): Option[scala.xml.Node] = (x, attr.value) match {
    case (node: Elem, Some(v)) =>
      val meta = new UnprefixedAttribute(attr.key.key, v, scala.xml.Null)
      Some(new Elem(node.prefix, node.label, meta, node.scope, node.minimizeEmpty, body.map(_.source): _*))
    case _ =>
      None
  }

  def copy(prefix: String = x.prefix, label: String = x.label,
           scope: NamespaceBinding = x.scope, meta: MetaData = x.attributes,
           body: Seq[scala.xml.Node] = x.child): x.type = {
    new Elem(prefix, label, meta, scope, x.asInstanceOf[Elem].minimizeEmpty, body: _*).asInstanceOf[x.type]
  }

}

class MetaDataOps(val x: scala.xml.MetaData) extends AnyVal {
  // Note: retrieval of attributes API is seriously broken in scala.xml
  // therefore we workaround the problem by mapping them to the attribute map
  // and checking their keys

  def getTag(tag: XmlAttribute): Option[String] =
    genericGetTag(s"${tag.namespace.short}:${tag.tag}")

  private def genericGetTag(entry: String): Option[String] =
    x.asAttrMap.get(entry)

  def fromTags(tags: List[(XmlAttribute, String)]): scala.xml.MetaData =
    tags.foldLeft(x) { case (acc, (attr, v)) => attr attributeWithValue (v, acc) }

  def copyWith(tagName: XmlAttribute, value: String): scala.xml.MetaData = {
    if (x == scala.xml.Null)
      new PrefixedAttribute(tagName.namespace.short.value, tagName.tag, value, scala.xml.Null)
    else if (x.key == tagName.tag)
      new PrefixedAttribute(tagName.namespace.short.value, tagName.tag, value, x.next)
    else
      x.copy(x.copyWith(tagName, value))
  }

}
