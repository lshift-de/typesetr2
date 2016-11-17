package net.lshift.typesetr
package pandoc.writers.latex

/**
 * Utility functions for generating LaTeX code.
 */
abstract class LatexTools {

  /**
   * Returns a latex-encoded quoted value
   *
   * @param s value to be quoted
   * @return a latex representation of the quotation
   */
  def quote(s: String): String

  /**
   * Returns a latex command to be used in the
   * final document
   *
   * @param name name of the command
   * @param opts command options
   * @param args command arguments
   * @return a latex representation of the command
   */
  def cmd(name: String, opts: List[String], args: List[String]): String

}

object LatexTools {

  /**
   * A default implementation of LaTeXTools that
   * is available in the implicit search scope.
   */
  implicit lazy val texTools: LatexTools = new LatexToolsImpl

}

private class LatexToolsImpl extends LatexTools {

  private final val unicodeMap: Map[Char, String] = Map(
    '\u0023' -> "\\#",
    '\u0024' -> "\\$",
    '\u0025' -> "\\%",
    '\u0026' -> "\\&",
    '\u003C' -> "{\\textless}",
    '\u003E' -> "{\\textgreater}",
    '\\' -> "{\\textbackslash}",
    '\u005E' -> "\\^{}",
    '\u0020' -> "\\_",
    '\u005F' -> "\\`{}",
    '\u0060' -> "\\`{}",
    '\u007B' -> "\\{",
    '\u007C' -> "{\\textbar}",
    '\u007D' -> "\\}",
    '\u007E' -> "{\\textasciitilde}",
    '\u005B' -> "{[}",
    '\u005D' -> "{]}")

  private final val unicodeRegex = unicodeMap.keys.mkString("[", "", "]").r

  def quote(s: String): String =
    unicodeRegex.replaceAllIn(s, m => unicodeMap(m.matched.charAt(0)))

  def cmd(name: String, opts: List[String], args: List[String]): String = {
    val opts1 = if (opts.isEmpty) "" else opts.mkString("[", ",", "]")
    val args1 = if (args.isEmpty) "{}" else args.map(arg => s"{$arg}").mkString("")
    s"\\\\$name${opts1}${args1}"
  }

}
