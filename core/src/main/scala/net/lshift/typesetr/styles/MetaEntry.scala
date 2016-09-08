package net.lshift.typesetr
package styles

import pandoc.writers.latex.LatexTools
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

import scalaz.Tags.First
import scalaz.Scalaz._

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

  def xmpEncoded(key: MetaKey)(implicit tools: LatexTools): String =
    s"pdf${key.rawName}={${toLatex}}"

  def raw: String = value.toString

  def toLatex(implicit tools: LatexTools): String

}

object MetaEntry {

  class MetaEntryWithSchema[T](val entry: LatexFormatter.Aux[T]) extends MetaEntry {

    type T = entry.T

    def value: MetaEntryWithSchema.this.T = entry.value

    def toLatex(implicit tools: LatexTools): String = entry.toLatex

  }

  def apply(metaKey: MetaKey, rawMetaEntry: Option[AnyRef],
            inferredMetaValue: Option[String],
            nonRequired: Boolean = false): Option[MetaEntry] = {
    // Retrieve the schema from the metadata entry
    val schema = meta.EntrySchema.apply(rawMetaEntry)
    // Retrieve the type of the entry from the schema
    val entryType = schema.`type`.getOrElse(meta.EntrySchema.defaultType)

    // a) we inferred the value from the document
    // b) we use the default value from the meta definition
    // c) we use the default value for the type of the entry
    val v = scalaz.Tag.unwrap(
      First(inferredMetaValue) |+|
        First(schema.default) |+|
        First(if (nonRequired || schema.isRequired) Some(entryType.default) else None))

    v map { v =>
      new MetaEntryWithSchema(
        if (metaKey.isCommaSeparated) ListEntry(v.toString)
        else NonListEntry[entryType.T](entryType.translate(v), entryType))
    }
  }

}

/**
 * Depending on the kind of the value of the entry
 * (list or not) we might need to do some necessary
 * text formatting magic in order to make it latex-conformant.
 */
sealed abstract class LatexFormatter {

  // The underlying type of the meta entry
  type T

  // The value of the meta entry
  def value: T

  /**
   * Convert the value to latex-conformant text
   * @param tools typeclass encapsulating the latex-magic formatting
   */
  def toLatex(implicit tools: LatexTools): String

}

object LatexFormatter {

  type Aux[S] = LatexFormatter { type T = S }

}

case class NonListEntry[A](value: A, entryKind: meta.MetaEntryType.Aux[A]) extends LatexFormatter {

  type T = A

  def toLatex(implicit tools: LatexTools): String = entryKind.latexify(value).replace(';', ',')

}

case class ListEntry(value0: String) extends LatexFormatter {
  import ListEntry._

  def value: List[String] = value0.split(',').toList

  type T = List[String]

  def stringify(implicit tools: LatexTools): String =
    value match {
      case _ :: _ :: _ =>
        val v = value0.replace(',', PRIVATE).replace(';', ',')
        val v2 = tools.quote(v).replaceAll(PRIVATE.toString, XMPCOMMA)
        tools.cmd(XMPQUOTE, Nil, List(v2))
      case _ =>
        value.map(_.replace(';', ',')).mkString("")
    }

  def toLatex(implicit tools: LatexTools): String = stringify

}

object ListEntry {

  private final val PRIVATE = '\ue001'

  private final val XMPCOMMA = "\\xmpcomma{}"

  private final val XMPQUOTE = "xmpquote"

}
