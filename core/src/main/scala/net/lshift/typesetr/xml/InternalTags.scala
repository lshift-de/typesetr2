package net.lshift.typesetr.xml

object InternalTags {
  final lazy val TITLE = Tag("title")
  final lazy val SUBTITLE = Tag("sub")
  final lazy val A = Tag("a")
  final lazy val LABEL = Tag("label")
  final lazy val SPAN = Tag("span")
  final lazy val B = Tag("b")
  final lazy val I = Tag("i")
  final lazy val S = Tag("s")
  final lazy val U = Tag("u")
  final lazy val SUP = Tag("sup")
  final lazy val SUB = Tag("sub")
  final lazy val SMALL = Tag("small")
  final lazy val BLOCKCODE = Tag("blockcode")
  final lazy val CODE = Tag("code")
  final lazy val CITE = Tag("cite")
  final lazy val Q = Tag("q")
  final lazy val WBR = Tag("wbr")

  final lazy val H1 = Tag("h1")
  final lazy val H2 = Tag("h2")
  final lazy val H3 = Tag("h3")
  final lazy val H4 = Tag("h4")
  final lazy val H5 = Tag("h5")
  final lazy val H6 = Tag("h6")

  final lazy val CAPTION = Tag("caption")
  final lazy val COLGROUP = Tag("colgroup")
  final lazy val COL = Tag("col")
  final lazy val TH = Tag("th")
  final lazy val TR = Tag("tr")
  final lazy val TD = Tag("td")

  final lazy val P = Tag("p")
  final lazy val ASIDE = Tag("aside")
  final lazy val OUTPUT = Tag("output")

  final lazy val DL = Tag("dl")
  final lazy val OL = Tag("ol")
  final lazy val UL = Tag("ul")
  final lazy val LI = Tag("li")
  final lazy val DT = Tag("dt")
  final lazy val DD = Tag("dd")
  final lazy val SECTION = Tag("section")
  final lazy val FOOTER = Tag("footer")
  final lazy val HEADER = Tag("header")
  final lazy val HGROUP = Tag("hgroup")
  final lazy val BLOCKQUOTE = Tag("blockquote")
  final lazy val FIGURE = Tag("figure")
  final lazy val FIGCAPTION = Tag("figcaption")
  final lazy val PRE = Tag("pre")
  final lazy val TABLE = Tag("table")
  final lazy val TFOOT = Tag("tfoot")

  final lazy val HR = Tag("hr")
  final lazy val BR = Tag("br")
  final lazy val DIV = Tag("div")
  final lazy val IMG = Tag("img")
  final lazy val FRAME = Tag("frame")

  final lazy val HEAD = Tag("head")
  final lazy val META = Tag("meta")
  final lazy val BODY = Tag("body")

  // ----------------
  // TODO:
  // For now we just ignore footnotes.
  final lazy val FOOTNOTE = Tag(".footnote")
  final lazy val PAGEBREAK = Tag(".pagebreak")
  final lazy val BLOCK = Tag(".block")

  final lazy val LIT = Tag("lit")
  final lazy val CMD = Tag("cmd")
  final lazy val LIST = Tag("list")

  final lazy val ROOT = Tag("root")
}
