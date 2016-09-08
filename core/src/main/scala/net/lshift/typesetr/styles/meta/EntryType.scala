package net.lshift.typesetr
package styles
package meta

import pandoc.writers.latex.LatexTools

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

/**
 * A schema class for translating string values into
 * appropriate Scala classes.
 */
sealed abstract class MetaEntryType {

  /**
   * The Scala type representing the value
   */
  type T

  /**
   * Translate a raw value into a first class Scala object
   * @param x a raw, untyped value of the meta entry
   * @return A first class Scala object
   */
  def translate(x: AnyRef): T

  /**
   * A default Scala object for type `T`
   * @return
   */
  def default: String

  /**
   * Converts the value into a latex-conformant text
   */
  def latexify(v: T): String

  /**
   * Human-readable type of the meta-entry
   */
  val name: String

}

object MetaEntryType {

  type Aux[S] = MetaEntryType { type T = S }

  case object BooleanEntry extends MetaEntryType {

    type T = Boolean

    def translate(x: AnyRef): Boolean = x match {
      case "yes" => true
      case "no"  => false
      case _     => ???
    }

    def default: String = "no"

    def latexify(v: Boolean): String = if (v) "yes" else "no"

    val name: String = "boolean"

  }

  case object TextEntry extends MetaEntryType {

    type T = String

    def translate(x: AnyRef): String = x.toString

    def default: String = ""

    def latexify(v: String): String = v

    val name: String = "text"

  }

  case object DateEntry extends MetaEntryType {

    private final val dateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")

    type T = DateTime

    def translate(x: AnyRef): DateTime = {
      if (x == default) new DateTime()
      else dateFormatter.parseDateTime(x.toString)
    }

    def default: String = "today"

    def latexify(v: DateTime): String = s"\\{\\\\origdate\\\\printdate\\{${dateFormatter.print(v)}\\}\\}"

    val name: String = "date"

  }

}