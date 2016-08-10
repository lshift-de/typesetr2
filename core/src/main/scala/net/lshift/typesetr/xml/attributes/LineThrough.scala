package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class LineThrough(val name: String)

object LineThrough {
  case object Solid extends LineThrough("solid")
  case object None extends LineThrough("none")

  implicit def stringToLineThrough(v: String): LineThrough = v match {
    case Solid.name => Solid
    case None.name  => None
  }
}

