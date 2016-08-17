package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class Underline(val name: String) extends StyleAttribute

object Underline {
  case object Solid extends Underline("solid")
  case object None extends Underline("none")

  implicit def stringToUnderline(v: String): Option[Underline] = v match {
    case Solid.name => Some(Solid)
    case None.name  => Some(None)
    case _          => scala.None
  }

}

