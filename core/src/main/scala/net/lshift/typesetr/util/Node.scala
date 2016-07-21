package net.lshift.typesetr
package util

import scala.annotation.tailrec
import scala.xml._
import xml.Tag
import xml.Attribute

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

}