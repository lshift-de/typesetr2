package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class ParBreak(val v: String) extends StyleAttribute {

  def name: String = v

}

object ParBreak {

  implicit def stringToParBreak(x: String): Option[ParBreak] = x match {
    case Auto.v     => Some(Auto)
    case Column.v   => Some(Column)
    case Page.v     => Some(Page)
    case EvenPage.v => Some(EvenPage)
    case OddPage.v  => Some(OddPage)
    case _          => None
  }

  case object Auto extends ParBreak("auto")
  case object Column extends ParBreak("column")
  case object Page extends ParBreak("page")
  case object EvenPage extends ParBreak("even-page")
  case object OddPage extends ParBreak("odd-page")

  def all = List(Auto, Column, Page, EvenPage, OddPage)

}
