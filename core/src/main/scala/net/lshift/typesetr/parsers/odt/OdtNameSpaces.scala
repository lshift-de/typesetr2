package net.lshift.typesetr.parsers.odt

import net.lshift.typesetr.xml.{ NameSpace, NameSpaceKey, NameSpaces }

import scala.language.postfixOps

class OdtNameSpaces(mapping: Map[String, String]) extends NameSpaces {
  def apply(key: NameSpaceKey): Option[NameSpace] =
    mapping.get(key.value).map(NameSpace)

  def name: String = "odt"

  protected def iterator: Iterable[(NameSpaceKey, NameSpace)] =
    mapping.map {
      case (key, value) =>
        (NameSpaceKey(key), NameSpace(value))
    } toIterable
}

object OdtNameSpaces {
  def apply(mapping: Map[String, String]): NameSpaces =
    new OdtNameSpaces(mapping)
}
