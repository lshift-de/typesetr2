package net.lshift.typesetr.xml

import scala.language.implicitConversions

abstract class Tag {
  def name: String

  // 'flagged' nodes should never be optimized,
  // and are taken as-is
  def flagged: Boolean
}

object Tag {

  def apply(name: String): Tag = InternalTag(name)

  lazy val nodeTag = Tag("node")

  lazy val textTag = Tag("text")

  lazy val syntheticTextTag = Tag("synth-text")

  implicit def toString(x: Tag): String = x.name

  implicit def toTag(x: String): Tag = Tag(x)

  implicit def toTagOps(x: Tag): TagOps =
    new TagOps(x)

  class TagOps(val x: Tag) extends AnyVal {

    def isIn(tags: List[Tag]): Boolean =
      tags.contains(x)
    def isIn(tag: Tag): Boolean =
      tag == x

    def |(y: Tag): List[Tag] = List(x, y)

  }

  implicit def toTagsOps(xs: List[Tag]): TagsOps =
    new TagsOps(xs)

  class TagsOps(val xs: List[Tag]) extends AnyVal {

    def |(y: Tag): List[Tag] = y :: xs

  }

  case class InternalTag(name: String) extends Tag {
    def flagged: Boolean = false
  }

  case class InternalTagWithNS(ns: String, name: String) extends Tag {
    def flagged: Boolean = true
  }

}
