package net.lshift.typesetr.xml

case class XmlTag(namespace: NameSpace, tag: String)

object XmlTag {
  implicit def fromRawToOdtTag(info: (Option[NameSpace], String)): XmlTag =
    if (info._1.isEmpty) throw new IllegalArgumentException(s"Invalid odt tag ${info._1} for ${info._2}")
    else XmlTag(info._1.get, info._2)

  implicit def strRepr(tag: XmlTag): String =
    s"${tag.namespace}:${tag.tag}"

  implicit class XmlTagOps(val t: XmlTag) extends AnyVal {

    def /(t2: XmlTag): XmlPath = XmlPath(Some(XmlPath(None, t)), t2)

    def toInternalTag: Tag = Tag.InternalTagWithNS(t.namespace.v, t.tag)

  }
}

case class XmlPath(prefix: Option[XmlPath], tag: XmlTag)

object XmlPath {
  implicit class XmlPathOps(val p: XmlPath) extends AnyVal {
    def /(t: XmlTag): XmlPath = XmlPath(Some(p), t)
  }
}
