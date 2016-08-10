package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class TextAlign(val align: String)

object TextAlign {

  implicit def stringToTextAlign(x: String): TextAlign = x match {
    case Start.align   => Start
    case End.align     => End
    case Left.align    => Left
    case Right.align   => Right
    case Center.align  => Center
    case Justify.align => Justify
  }

  case object Start extends TextAlign("start")
  case object End extends TextAlign("end")
  case object Left extends TextAlign("left")
  case object Right extends TextAlign("right")
  case object Center extends TextAlign("center")
  case object Justify extends TextAlign("justify")

  def all = List(Start, End, Left, Right, Center, Justify)

}
