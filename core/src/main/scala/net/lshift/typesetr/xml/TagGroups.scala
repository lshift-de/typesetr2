package net.lshift.typesetr.xml

import net.lshift.typesetr.xml.Attributes._
import net.lshift.typesetr.xml.InternalTags._

object TagGroups {
  final val PSEUDO_BLOCK_TAGS = List(FOOTNOTE, PAGEBREAK, TITLE, SUBTITLE)
  final val PSEUDO_INLINE_TAGS = List()

  final val INLINE_TAG = List(SPAN, A,
    B, I, S, U,
    //"em", "strong", "mark",
    SUP, SUB, SMALL,
    BLOCKCODE,
    //"kbd", "var", "samp",
    //"abbr", "time",
    CITE, Q,
    //"dfn",
    WBR) ++ PSEUDO_INLINE_TAGS
  final val H_TAGS = List(H1, H2, H3, H4, H5, H6)
  final val INLINE_BLOCK_TAGS: List[String] = List() //List("data", "del", "ins")
  final val TABLE_TAGS = List(
    //"thead", "tbody",
    CAPTION, COLGROUP, COL,
    TH, TR, TD)

  final val NON_EMPTY_BLOCK_TAGS = H_TAGS ++ List(
    DL, OL, UL,
    LI, DT, DD,
    SECTION, FOOTER, HEADER, HGROUP,
    P, ASIDE, //OUTPUT,
    //"noscript",
    BLOCKQUOTE,
    FIGURE,
    FIGCAPTION,
    PRE,
    TABLE, TFOOT //"output",
    //"form", "fieldset"
    )

  // XXX: main reason div is here is for `<div class="pagebreak"/>`,
  final val BLOCK_TAGS =
    NON_EMPTY_BLOCK_TAGS ++ List(HR, BR, DIV) ++ TABLE_TAGS ++ PSEUDO_BLOCK_TAGS
  final val MEDIA_TAGS = List(IMG // "object",
  // "audio", "video", "canvas",
  )

  final val ALLOWED_TAGS = INLINE_TAG ++ BLOCK_TAGS ++
    MEDIA_TAGS ++ List(
      HEAD, META, BODY)

  final val COLOR_TYPES = List(COLOR, BACKGROUND_COLOR)

  // fully void = no body, and, optionally, not attrs either
  final val FULLY_VOID_TAGS = List(HR, BR, WBR) ++
    TABLE_TAGS ++
    List(PAGEBREAK, FIGCAPTION, CAPTION)

  final val INLINE_TAG_WITH_BLOCKQUOTE = BLOCKQUOTE :: INLINE_TAG

  final val NEVER_BLANK = List(FIGURE, IMG, CMD, LIT, PAGEBREAK)

  final val CAN_OCCUR_IN_HEADER =
    List(A, FOOTNOTE, ASIDE,
      // allow underlining, to ensure we don't strip out CMDs and
      // LITs in headings before we get around to calling
      // `underlines_to_commands` on the heading)
      U, CMD, LIT,
      // margin figures
      FIGURE, IMG)
}
