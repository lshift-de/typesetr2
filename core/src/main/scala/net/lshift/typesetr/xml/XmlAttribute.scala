package net.lshift.typesetr.xml

import scala.language.implicitConversions
import scala.xml.{ UnprefixedAttribute, MetaData }

case class XmlAttribute(namespace: NameSpace, tag: String) {

  override def toString: String = s"$namespace:$tag"

}

object XmlAttribute {

  implicit def fromRawToOdtTag(info: (Option[NameSpace], String)): XmlAttribute =
    if (info._1.isEmpty) throw new IllegalArgumentException(s"Invalid xml tag ${info._1} for ${info._2}")
    else XmlAttribute(info._1.get, info._2)

  implicit class XmlAttributeOps(val t: XmlAttribute) extends AnyVal {

    def attributeWithValue(value: String, prev: MetaData): MetaData =
      new UnprefixedAttribute(t.toString, value, prev)

  }

}