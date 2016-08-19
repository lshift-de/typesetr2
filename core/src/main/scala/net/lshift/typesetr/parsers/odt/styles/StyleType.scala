package net.lshift.typesetr
package parsers.odt.styles

import net.lshift.typesetr.xml.attributes.FontFamily
import xml.Tag
import xml.{ InternalTags => ITags }

import scala.language.implicitConversions

/*
 * Represents a `type` of the style.
 *
 *  The type is taken from the original ODT format
 *  and translated into a generic output node style.
 */
sealed abstract class StyleType(val name: String) {
  def tag: Tag
}

object StyleType {

  def all = List(TitleStyleTpe, SubTitleStyleTpe,
                 HeadingStyle.h1, HeadingStyle.h2, HeadingStyle.h3, HeadingStyle.h4,
                 HeadingStyle.h5, HeadingStyle.h6,
                 ListStyleTpe, // can"t use "ol"", "ul", because we"ll find out later
                 PStyleTpe, SpanStyleTpe, TableTpeStyle, TableRowTpeStyle,
                 TableCellTpeStyle, TableColTpeStyle)

}

case object ListStyleTpe extends StyleType("list") {

  def tag: Tag = ITags.LIST

  override def toString: String = s"<$name>"

}

class HeadingStyle(val index: Int) extends StyleType(s"h$index") {

  def tag: Tag = index match {
    case 1 => ITags.H1
    case 2 => ITags.H2
    case 3 => ITags.H3
    case 4 => ITags.H4
    case 5 => ITags.H5
    case 6 => ITags.H6
  }

  def hashcode(): Int = 7 * index.hashCode

  override def equals(x: Any): Boolean = x match {
    case h: HeadingStyle => h.index == index
    case _               => false
  }

  override def toString: String =
    s"<h$index>"
}

object HeadingStyle {

  def unapply(v: String): Option[StyleType] = v match {
    case regex(id) => Some(new HeadingStyle(id.toInt))
    case _         => None
  }

  def apply(idx: Int): HeadingStyle = {
    assert(idx > 0 && idx < 7)
    new HeadingStyle(idx)
  }

  private val regex = """Heading ([1-6]).*""".r

  lazy val h1 = HeadingStyle(1)
  lazy val h2 = HeadingStyle(2)
  lazy val h3 = HeadingStyle(3)
  lazy val h4 = HeadingStyle(4)
  lazy val h5 = HeadingStyle(5)
  lazy val h6 = HeadingStyle(6)

}

case object PStyleTpe extends StyleType("p") {
  def tag: Tag = ITags.P
  override def toString: String = s"<$name>"
}

case object SpanStyleTpe extends StyleType("span") {
  def tag: Tag = ITags.SPAN
  override def toString: String = s"<$name>"
}

sealed abstract class TitleStyleType(name: String, val init: String) extends StyleType(name)

object TitleStyleType {

  def unapply(v: String): Option[StyleType] = v match {
    case TitleStyleTpe.init    => Some(TitleStyleTpe)
    case SubTitleStyleTpe.init => Some(SubTitleStyleTpe)
    case _                     => None
  }

}

case object TitleStyleTpe extends TitleStyleType("title", "Title") {

  def tag: Tag = ITags.TITLE

  override def toString: String = s"<$name>"

}

case object SubTitleStyleTpe extends TitleStyleType("subtitle", "Subtitle") {

  def tag: Tag = ITags.SUBTITLE

  override def toString: String = s"<$name>"

}

sealed abstract class TableStyleType(name: String, val init: String, val tag: Tag)
  extends StyleType(name) {

  override def toString: String = s"<table:$name>"

}

object TableStyleType {

  def unapply(v: String): Option[StyleType] = v match {
    case TableTpeStyle.init     => Some(TableTpeStyle)
    case TableCellTpeStyle.init => Some(TableCellTpeStyle)
    case TableRowTpeStyle.init  => Some(TableRowTpeStyle)
    case TableColTpeStyle.init  => Some(TableColTpeStyle)
    case _                      => None
  }

}

case object TableTpeStyle extends TableStyleType("table", "table", ITags.TABLE)
case object TableCellTpeStyle extends TableStyleType("td", "table-cell", ITags.TD)
case object TableRowTpeStyle extends TableStyleType("tr", "table-row", ITags.TR)
case object TableColTpeStyle extends TableStyleType("col", "table-column", ITags.COL)

case class SubListStyle(level: Int, prop: SubListProps)
case class SubListProps(fontName: FontFamily)