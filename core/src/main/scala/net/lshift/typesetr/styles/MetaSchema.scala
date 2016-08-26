package net.lshift.typesetr
package styles

import java.util.Date

import net.lshift.typesetr.pandoc.writers.latex.{ XMPMeta, LatexTools }
import org.yaml.snakeyaml.Yaml

import scala.collection.{ mutable, GenTraversable }
import scala.collection.generic.FilterMonadic
import scala.collection.JavaConversions.mapAsScalaMap

/**
 * Provides an iterable meta-schema, loaded from the template's meta info files.
 *
 * Iterable version of MetaSchema ignores a potential language setting.
 * MetaSchema returns (MetaKey, MetaEntry) tuples. The latter can be
 * of different types: plain string values, lists, maps etc.
 */
abstract class MetaSchema {

  def getKey(key: MetaKey): Option[MetaEntry]

  // The next two methods make it for-comprehensible
  // TODO: add `map` if needed.
  def flatMap[B](f: ((MetaKey, MetaEntry)) => Option[B]): Iterable[B]

  def withFilter(p: ((MetaKey, MetaEntry)) => Boolean): MetaSchema
}

object MetaSchema {

  def apply(f: java.io.File): Option[MetaSchema] =
    if (f.exists) {
      val yaml = new Yaml()
      val map = (for {
        obj <- f.loadFile()
      } yield mapAsScalaMap(yaml.load(obj).asInstanceOf[java.util.LinkedHashMap[String, AnyRef]]).toMap)

      Some(new MetaSchemaFromMap(map.getOrElse(Map.empty)))
    } else None

  class MetaSchemaFromMap(map: Map[String, AnyRef]) extends MetaSchema {

    // FIXME: needs to handle fallbacks
    def getKey(key: MetaKey): Option[MetaEntry] =
      map.get(key.name).flatMap(MetaEntry.apply)

    def flatMap[B](f: ((MetaKey, MetaEntry)) => Option[B]): Iterable[B] =
      map.flatMap {
        case (k, v) =>
          MetaEntry(v).flatMap(v => f((MetaKey(k), v)))
      }

    def withFilter(p: ((MetaKey, MetaEntry)) => Boolean): MetaSchema = {
      val filtered = map.filter {
        case (k, v) =>
          if (v == null) false
          else MetaEntry(v).map(v => p((MetaKey(k), v))).getOrElse(false)
      }
      new MetaSchemaFromMap(filtered)
    }

    private lazy val nonLangEntries = {
      map.toList.flatMap({
        case (key, v) =>
          // TODO: get rid of the fixed string
          if (key != "lang") {
            MetaEntry(v).map(v => (MetaKey(key), v))
          } else None
      })
    }

    // TODO: better preatty print
    override def toString: String =
      s"""|MetaSchema:
          |$map""".stripMargin

  }

}

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
}

object MetaEntry {
  def apply(x: AnyRef): Option[MetaEntry] = x match {
    case x: java.util.LinkedHashMap[String, AnyRef] =>
      Some(MapEntry(mapAsScalaMap[String, AnyRef](x).toMap))
    case _ =>
      println(s"Missing meta entry for $x of type ${x.getClass}")
      ???
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
}

case class StringEntry(value: String) extends MetaEntry {

  type T = String

  def hasMultipleValues: Boolean = false

  protected def stringify(implicit tools: LatexTools): String = value.replace(';', ',')

  def toLatex: String = value
}

case class MapEntry(value: Map[String, AnyRef]) extends MetaEntry {

  type T = Map[String, AnyRef]

  def hasMultipleValues: Boolean = value.size > 1

  protected def stringify(implicit tools: LatexTools): String =
    // FIXME:
    "FIXME"

  def toLatex: String = "FIXME"

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

  def toLatex: String = "FIXME"
}

object ListEntry {

  private final val PRIVATE = '\ue001'

  private final val XMPCOMMA = "\\xmpcomma{}"

  private final val XMPQUOTE = "xmpquote"

}
