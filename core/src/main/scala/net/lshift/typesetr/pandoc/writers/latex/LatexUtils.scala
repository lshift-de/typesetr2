package net.lshift.typesetr
package pandoc.writers.latex

abstract class LatexTools {

  def quote(s: String): String
  def cmd(name: String, opts: List[String], args: List[String]): String

}

object LatexTools {

  implicit def utils: LatexTools = LatexUtils

}

object LatexUtils extends LatexTools {

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
