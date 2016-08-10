package net.lshift.typesetr.xml.attributes

import scala.language.implicitConversions

abstract class ParBreak(val v: String)

object ParBreak {

  implicit def stringToParBreak(x: String): ParBreak = x match {
    case Auto.v     => Auto
    case Column.v   => Column
    case Page.v     => Page
    case EvenPage.v => EvenPage
    case OddPage.v  => OddPage
  }

  case object Auto extends ParBreak("auto")
  case object Column extends ParBreak("column")
  case object Page extends ParBreak("page")
  case object EvenPage extends ParBreak("even-page")
  case object OddPage extends ParBreak("odd-page")

  def all = List(Auto, Column, Page, EvenPage, OddPage)

}
