package net.lshift.typesetr.xml

import scala.language.implicitConversions

case class XmlPath(prefix: Option[XmlPath], tag: XmlTag)

object XmlPath {
  implicit class XmlPathOps(val p: XmlPath) extends AnyVal {
    def /(t: XmlTag): XmlPath = XmlPath(Some(p), t)
  }
}
