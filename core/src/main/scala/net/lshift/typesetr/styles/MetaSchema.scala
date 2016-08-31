package net.lshift.typesetr
package styles

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
