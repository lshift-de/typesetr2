package net.lshift.typesetr
package styles

import java.util.Date

import pandoc.writers.latex.LatexTools

import scala.collection.JavaConversions._

/**
 * Class representing a meta info entry.
 *
 * We allow for a number of kinds of MetaEntry,
 * such as a plain string value, lists, maps etc.
 *
 */
abstract class MetaEntry {
  type T

  def value: T

  def hasMultipleValues: Boolean

  protected def stringify(implicit tools: LatexTools): String

  def xmpEncoded(key: MetaKey)(implicit tools: LatexTools): String =
    s"pdf${key.rawName}={${stringify}}"

  def raw: String = value.toString

  def toLatex: String

  def isRequired: Boolean
}

object MetaEntry {
  def apply(x: AnyRef): Option[MetaEntry] = (x: @unchecked) match {
    case x: String =>
      Some(StringEntry(x))
    case x: java.util.LinkedHashMap[String, AnyRef] =>
      Some(MapEntry(mapAsScalaMap[String, AnyRef](x).toMap))
    case _ =>
      if (x != null)
        println(s"Missing meta entry for $x of type ${x.getClass}")
      ???
  }

  def apply(x: AnyRef, inferred: Option[MetaEntry]): Option[MetaEntry] = {
    // todo: check type, required, etc
    if (inferred.nonEmpty) inferred
    else apply(x)
  }

  private final val defaultValueForMeta = Map(
    "text" -> "",
    "rich-text" -> "",
    "bibliography" -> "",
    "boolean" -> false,
    "date" -> new Date(),
    "image" -> "",
    "multiline" -> "",
    "lang" -> Lang.default)

  protected final val fieldsTranslation: Map[String, MetaEntryType] = Map(
    "required" -> MetaEntryType.BooleanEntry)

  final val reqField = "required"

}

case class StringEntry(value: String) extends MetaEntry {

  type T = String

  def hasMultipleValues: Boolean = false

  protected def stringify(implicit tools: LatexTools): String = value.replace(';', ',')

  def toLatex: String = value

  def isRequired: Boolean = false

}

case class MapEntry(value: Map[String, AnyRef]) extends MetaEntry {

  type T = Map[String, AnyRef]

  def hasMultipleValues: Boolean = value.size > 1

  protected def stringify(implicit tools: LatexTools): String =
    "FIXME1"

  def toLatex: String = "FIXME1"

  def isRequired: Boolean =
    value.get(MetaEntry.reqField).map(
      MetaEntryType.BooleanEntry.translate).getOrElse(false)

}

case class ListEntry(value: List[String]) extends MetaEntry {
  import ListEntry._

  type T = List[String]

  def hasMultipleValues: Boolean = value match {
    case _ :: _ :: _ => true
    case _           => false
  }

  protected def stringify(implicit tools: LatexTools): String =
    value match {
      case _ :: _ :: _ if hasMultipleValues =>
        val v = value.mkString(",").replace(',', PRIVATE).replace(';', ',')
        val v2 = tools.quote(v).replaceAll(PRIVATE.toString, XMPCOMMA)
        tools.cmd(XMPQUOTE, Nil, List(v2))
      case _ =>
        value.map(_.replace(';', ',')).mkString("")
    }

  def toLatex: String = "FIXME2"

  def isRequired: Boolean = false
}

object ListEntry {

  private final val PRIVATE = '\ue001'

  private final val XMPCOMMA = "\\xmpcomma{}"

  private final val XMPQUOTE = "xmpquote"

}

sealed abstract class MetaEntryType {

  type T

  def translate(x: AnyRef): T

  def default: T

}

object MetaEntryType {

  case object BooleanEntry extends MetaEntryType {

    type T = Boolean

    def translate(x: AnyRef): Boolean = x match {
      case "yes" => true
      case "no"  => false
      case _     => ???
    }

    def default: Boolean = false

  }

  case object StringEntry extends MetaEntryType {

    type T = String

    def translate(x: AnyRef): String = x.toString

    def default: String = ""

  }

  case object ListEntry extends MetaEntryType {

    type T = List[String]

    def translate(x: AnyRef): List[String] =
      x.toString.split(",").toList.map(_.trim)

    def default: List[String] = List()

  }

}