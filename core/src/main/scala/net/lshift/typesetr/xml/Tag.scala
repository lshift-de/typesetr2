package net.lshift.typesetr.xml

import scala.language.implicitConversions

case class Tag(name: String)

object Tag {

  implicit def toString(x: Tag): String = x.name

  implicit def toTag(x: String): Tag = Tag(x)

  implicit def toTagOps(x: Tag): TagOps = new TagOps(x)

  class TagOps(val x: Tag) extends AnyVal {
    def isIn(tags: List[Tag]): Boolean =
      tags.contains(x)
    def isIn(tag: Tag): Boolean =
      tag == x
  }

}
