package net.lshift.typesetr
package parsers
package odt

import styles.Style

abstract class ParseContext {
  def listLevel: Int
  def listType: Option[ListType]
  def style: Option[Style]
}

sealed abstract class ListType {
  def values: List[String]
  def css: List[String]
}

case object Enumerations extends ListType {
  override def values: List[String] = ???

  override def css: List[String] = ???
}

case object Bullets extends ListType {
  override def values: List[String] = ???

  override def css: List[String] = ???
}
