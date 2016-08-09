package net.lshift.typesetr.xml

case class NameSpaceKey(value: String)

object NameSpaceKey {
  implicit def keyFromString(v: String): NameSpaceKey =
    NameSpaceKey(v)
}
