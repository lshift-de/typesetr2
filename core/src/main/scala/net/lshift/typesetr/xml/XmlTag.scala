package net.lshift.typesetr.xml

import scala.language.implicitConversions
import scala.xml.{ UnprefixedAttribute, MetaData }

case class XmlTag(namespace: NameSpace, tag: String) {

  override def toString: String = s"$namespace:$tag"

}

object XmlTag {

  implicit def fromRawToOdtTag(info: (Option[NameSpace], String)): XmlTag =
    if (info._1.isEmpty) throw new IllegalArgumentException(s"Invalid tag ${info._1} for ${info._2}")
    else XmlTag(info._1.get, info._2)

  implicit def strRepr(tag: XmlTag): String =
    s"${tag.namespace.short}:${tag.tag}"

  implicit class XmlTagOps(val t: XmlTag) extends AnyVal {

    def /(t2: XmlTag): XmlPath = XmlPath(Some(XmlPath(None, t)), t2)

    def toInternalTag: Tag = Tag.InternalTagWithNS(t.namespace.v, t.tag)

  }

}
