package net.lshift.typesetr.xml

import scala.language.implicitConversions

case class NameSpaceKey(value: String) {
  override def toString: String = value
}

object NameSpaceKey {
  implicit def keyFromString(v: String): NameSpaceKey = NameSpaceKey(v)
}
