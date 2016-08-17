package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class LineThrough(val name: String) extends StyleAttribute

object LineThrough {
  case object Solid extends LineThrough("solid")
  case object None extends LineThrough("none")

  implicit def stringToLineThrough(v: String): Option[LineThrough] = v match {
    case Solid.name => Some(Solid)
    case None.name  => Some(None)
    case _          => scala.None
  }

}

