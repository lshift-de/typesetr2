package net.lshift.typesetr
package styles

import org.yaml.snakeyaml.Yaml

import scala.collection.{ mutable, GenTraversable }
import scala.collection.generic.FilterMonadic
import scala.collection.JavaConversions.mapAsScalaMap

import scalaz.Tags.First
import scalaz.Scalaz._

/**
 * Provides an iterable meta-schema, loaded from the template's meta info files.
 *
 * Iterable version of MetaSchema ignores a potential language setting.
 * MetaSchema returns (MetaKey, MetaEntry) tuples. The latter can be
 * of different types: plain string values, lists, maps etc.
 */
abstract class MetaSchema { self =>

  def getKey(key: MetaKey): Option[MetaEntry]

  // The next two methods make it for-comprehensible
  // TODO: add `map` if needed.
  def flatMap[B](f: ((MetaKey, MetaEntry)) => Option[B]): Iterable[B]

  def withFilter(p: ((MetaKey, MetaEntry)) => Boolean): MetaSchema

  def attachDocumentMeta(meta: MetaFromDocument): self.type

}

object MetaSchema {

  def apply(f: java.io.File): Option[MetaSchema] =
    if (f.exists) {
      val yaml = new Yaml()
      val map = (for {
        obj <- f.loadFile()
      } yield mapAsScalaMap(yaml.load(obj).asInstanceOf[java.util.LinkedHashMap[String, AnyRef]]).toMap)

      Some(new MetaSchemaFromMap(map.getOrElse(Map.empty), None))
    } else None

  class MetaSchemaFromMap(map: Map[String, AnyRef], inferredMeta: Option[MetaFromDocument]) extends MetaSchema { self =>

    def getKey(key: MetaKey): Option[MetaEntry] =
      getKey(key, nonRequired = false)

    // FIXME: needs to handle fallbacks
    def getKey(key: MetaKey, nonRequired: Boolean): Option[MetaEntry] =
      MetaEntry.apply(key, map.get(key.name), inferredMetaValue = inferredFromDoc(key), nonRequired)

    private def allEntries: List[(MetaKey, MetaEntry)] = {
      val keyset = map.keySet.map(MetaKey.apply) ++ inferredMeta.map(_.entries.map(_._1)).getOrElse(Nil)
      (for {
        key <- keyset
        entry <- getKey(key, nonRequired = true)
      } yield (key, entry)).toList
    }

    def flatMap[B](f: ((MetaKey, MetaEntry)) => Option[B]): Iterable[B] =
      allEntries.flatMap { case (k, v) => f((k, v)) }

    def withFilter(p: ((MetaKey, MetaEntry)) => Boolean): MetaSchema = {
      val filtered = map.filter {
        case (k, v) =>
          if (v == null) false
          else {
            val mkey = MetaKey(k)
            MetaEntry(mkey, Some(v), inferredFromDoc(mkey), nonRequired = true).
              map(v => p((mkey, v))).getOrElse(false)
          }
      }
      new MetaSchemaFromMap(filtered, inferredMeta)
    }

    def attachDocumentMeta(meta: MetaFromDocument): self.type =
      new MetaSchemaFromMap(map, Some(meta)).asInstanceOf[self.type]

    private def inferredFromDoc(key: MetaKey): Option[String] =
      for {
        meta <- inferredMeta
        entry <- meta.fromKey(key)
      } yield entry

    private lazy val nonLangEntries = {
      map.toList.flatMap({
        case (key, v) =>
          // TODO: get rid of the fixed string
          if (!MetaSchemaFromMap.nonIterableEntries.contains(key)) {
            val mKey = MetaKey(key)
            MetaEntry(mKey, Some(v), inferredFromDoc(mKey)).map(v => (MetaKey(key), v))
          } else None
      })
    }

    // TODO: better pretty print
    override def toString: String =
      s"""|MetaSchema:
          |${allEntries.mkString("\n")}""".stripMargin

  }

  object MetaSchemaFromMap {
    private final val nonIterableEntries = List("lang")
  }

}
