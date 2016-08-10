package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class Underline(val name: String)

object Underline {
  case object Solid extends Underline("solid")
  case object None extends Underline("none")

  implicit def stringToUnderline(v: String): Underline = v match {
    case Solid.name => Solid
    case None.name  => None
  }
}

