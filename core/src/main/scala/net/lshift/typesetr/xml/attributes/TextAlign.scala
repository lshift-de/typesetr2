package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class TextAlign(val align: String) extends StyleAttribute {

  def name: String = align

}

object TextAlign {

  implicit def stringToTextAlign(x: String): Option[TextAlign] = x match {
    case Start.align   => Some(Start)
    case End.align     => Some(End)
    case Left.align    => Some(Left)
    case Right.align   => Some(Right)
    case Center.align  => Some(Center)
    case Justify.align => Some(Justify)
    case _             => None
  }

  case object Start extends TextAlign("start")
  case object End extends TextAlign("end")
  case object Left extends TextAlign("left")
  case object Right extends TextAlign("right")
  case object Center extends TextAlign("center")
  case object Justify extends TextAlign("justify")

  def all = List(Start, End, Left, Right, Center, Justify)

  implicit def toString(x: TextAlign): String = x.name

}
