package net.lshift.typesetr
package styles
package meta

/**
  * Meta schema that is read and interpreted from the metadata.yml files
  *
  * @param schema a raw map of fields vs values of the yaml file
  */
class EntrySchema(schema: Map[String, AnyRef]) {
  import EntrySchema._

  def `type`: Option[MetaEntryType] =
    schema.get("type").flatMap(tpe =>
      fieldsTpeTranslation.get(tpe.toString))

  def help: Option[String] =
    schema.get(helpField).map(_.toString)

  def default: Option[String] =
    schema.get(defaultField).map(_.toString)

  def label: Option[String] =
    schema.get(labelField).map(_.toString)

  def isRequired: Boolean =
    schema.get(reqField).map(
      MetaEntryType.BooleanEntry.translate).getOrElse(false)

}

object EntrySchema {

  def apply(x: Option[AnyRef]): EntrySchema =
    x flatMap {
      case x: String =>
        x.split(": ").toList match {
          case key :: value :: Nil =>
            Some(new EntrySchema(Map(key -> value)))
          case _ =>
            None
        }
      case x: java.util.LinkedHashMap[String, AnyRef] =>
        import scala.collection.JavaConversions._
        Some(new EntrySchema(mapAsScalaMap[String, AnyRef](x).toMap))
      case x =>
        if (x != null) {
          println(s"Missing meta entry for $x of type ${x.getClass}")
          ???
        } else {
          None
        }
    } getOrElse (new EntrySchema(Map.empty))

  def defaultType: MetaEntryType = MetaEntryType.TextEntry

  private final lazy val fieldsTpeTranslation: Map[String, MetaEntryType] = Map(
    MetaEntryType.BooleanEntry.name -> MetaEntryType.BooleanEntry,
    MetaEntryType.DateEntry.name -> MetaEntryType.DateEntry,
    MetaEntryType.TextEntry.name -> MetaEntryType.TextEntry)

  final val reqField = "required"

  final val helpField = "help"

  final val defaultField = "default"

  final val labelField = "label"

}