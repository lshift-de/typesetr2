package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class TextPosition(val name: String) extends StyleAttribute

object TextPosition {

  case object Sup extends TextPosition("super")
  case object Sub extends TextPosition("sub")
  case object None extends TextPosition("none")

  implicit def stringToTextPosition(v: String): TextPosition = v match {
    case Sub.name  => Sub
    case Sup.name  => Sup
    case None.name => None
  }

}
