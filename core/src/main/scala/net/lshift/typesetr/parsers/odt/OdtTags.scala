package net.lshift.typesetr
package parsers.odt

import xml.{ XmlTag, XmlAttribute, NameSpaces }

object OdtTags {
  // main tags
  val Document: XmlTag = (implicitly[NameSpaces].apply("office"), "document-content")
  val AutomaticStyle: XmlTag = (implicitly[NameSpaces].apply("office"), "automatic-styles")
  val Font: XmlTag = (implicitly[NameSpaces].apply("office"), "font-face-decls")
  val Body: XmlTag = (implicitly[NameSpaces].apply("office"), "body")
  val Text: XmlTag = (implicitly[NameSpaces].apply("office"), "text")
  val MasterStyle: XmlTag = (implicitly[NameSpaces].apply("office"), "master-styles")
  val Scripts: XmlTag = (implicitly[NameSpaces].apply("office"), "scripts")
  val Annotation: XmlTag = (implicitly[NameSpaces].apply("office"), "annotation")
  val Styles: XmlTag = (implicitly[NameSpaces].apply("office"), "styles")

  val Frame: XmlTag = (implicitly[NameSpaces].apply("draw"), "frame")
  val Image: XmlTag = (implicitly[NameSpaces].apply("draw"), "image")

  val A: XmlTag = (implicitly[NameSpaces].apply("text"), "a")
  val BookmarkStart: XmlTag = (implicitly[NameSpaces].apply("text"), "bookmark-start")
  val BookmarkEnd: XmlTag = (implicitly[NameSpaces].apply("text"), "bookmark-end")
  val Bookmark: XmlTag = (implicitly[NameSpaces].apply("text"), "bookmark")
  val H: XmlTag = (implicitly[NameSpaces].apply("text"), "h")
  val TextList: XmlTag = (implicitly[NameSpaces].apply("text"), "list")
  val TextListItem: XmlTag = (implicitly[NameSpaces].apply("text"), "list-item")
  val TextListStyle: XmlTag = (implicitly[NameSpaces].apply("text"), "list-style")
  val Note: XmlTag = (implicitly[NameSpaces].apply("text"), "note")
  val NoteBody: XmlTag = (implicitly[NameSpaces].apply("text"), "note-body")
  val NoteCitation: XmlTag = (implicitly[NameSpaces].apply("text"), "note-citation")
  val P: XmlTag = (implicitly[NameSpaces].apply("text"), "p")
  val S: XmlTag = (implicitly[NameSpaces].apply("text"), "s")
  val Span: XmlTag = (implicitly[NameSpaces].apply("text"), "span")
  val Tab: XmlTag = (implicitly[NameSpaces].apply("text"), "tab")
  val C: XmlAttribute = (implicitly[NameSpaces].apply("text"), "c")
  val TextNameAttr: XmlAttribute = (implicitly[NameSpaces].apply("text"), "name")
  val Linebreak: XmlTag = (implicitly[NameSpaces].apply("text"), "line-break")
  val SeqDecl: XmlTag = (implicitly[NameSpaces].apply("text"), "sequence-decls")
  val StyleNameAttr: XmlAttribute = (implicitly[NameSpaces].apply("text"), "style-name")
  val SoftPageBreak: XmlTag = (implicitly[NameSpaces].apply("text"), "soft-page-break")
  val TextOutlineStyle: XmlTag = (implicitly[NameSpaces].apply("text"), "outline-style")
  val TextNotesConf: XmlTag = (implicitly[NameSpaces].apply("text"), "notes-configuration")
  val TextLineNumConf: XmlTag = (implicitly[NameSpaces].apply("text"), "linenumbering-configuration")

  val TableStyleNameAttr: XmlAttribute = (implicitly[NameSpaces].apply("table"), "style-name")
  val Table: XmlTag = (implicitly[NameSpaces].apply("table"), "table")
  val TableColumn: XmlTag = (implicitly[NameSpaces].apply("table"), "table-column")
  val TableRow: XmlTag = (implicitly[NameSpaces].apply("table"), "table-row")
  val TableCell: XmlTag = (implicitly[NameSpaces].apply("table"), "table-cell")

  val Id: XmlTag = (implicitly[NameSpaces].apply("xml"), "id")

  val HrefAttr: XmlAttribute = (implicitly[NameSpaces].apply("xlink"), "href")
  val HrefType: XmlTag = (implicitly[NameSpaces].apply("xlink"), "type")

  val Creator: XmlTag = (implicitly[NameSpaces].apply("dc"), "creator")

  // style tags
  val MasterPage: XmlTag = (implicitly[NameSpaces].apply("style"), "master-page")
  val StyleDefault: XmlTag = (implicitly[NameSpaces].apply("style"), "default-style")
  val StyleHeader: XmlTag = (implicitly[NameSpaces].apply("style"), "header")
  val StyleFooter: XmlTag = (implicitly[NameSpaces].apply("style"), "footer")
  val StyleStyle: XmlTag = (implicitly[NameSpaces].apply("style"), "style")
  val StyleName: XmlAttribute = (implicitly[NameSpaces].apply("style"), "name")
  val StyleFamily: XmlAttribute = (implicitly[NameSpaces].apply("style"), "family")
  val StylePProps: XmlTag = (implicitly[NameSpaces].apply("style"), "paragraph-properties")
  val StyleTProps: XmlTag = (implicitly[NameSpaces].apply("style"), "text-properties")
  val StylePageLayout: XmlTag = (implicitly[NameSpaces].apply("style"), "page-layout")
  val StylePageLayoutProps: XmlTag = (implicitly[NameSpaces].apply("style"), "page-layout-properties")

  val StyleParentStyle: XmlAttribute = (implicitly[NameSpaces].apply("style"), "parent-style-name")
  val StyleDisplayName: XmlAttribute = (implicitly[NameSpaces].apply("style"), "display-name")
  val StyleTableColProps: XmlTag = (implicitly[NameSpaces].apply("style"), "table-column-properties")
  val StyleTableRowProps: XmlTag = (implicitly[NameSpaces].apply("style"), "table-row-properties")
  val StyleTableCellProps: XmlTag = (implicitly[NameSpaces].apply("style"), "table-cell-properties")
  val StyleTableProps: XmlTag = (implicitly[NameSpaces].apply("style"), "table-properties")
  val StyleFontName: XmlAttribute = (implicitly[NameSpaces].apply("style"), "font-name")
  val StyleTextUnderline: XmlAttribute = (implicitly[NameSpaces].apply("style"), "text-underline-style")
  val StyleTextLinethrough: XmlAttribute = (implicitly[NameSpaces].apply("style"), "text-line-through-style")
  val StyleTextPosition: XmlAttribute = (implicitly[NameSpaces].apply("style"), "text-position")
  val StyleColumnWidth: XmlAttribute = (implicitly[NameSpaces].apply("style"), "column-width")
  val StyleMinHeigh: XmlAttribute = (implicitly[NameSpaces].apply("style"), "min-row-height")
  val StyleFStyleAsian: XmlAttribute = (implicitly[NameSpaces].apply("style"), "font-style-asian")
  val StyleFStyleComplex: XmlAttribute = (implicitly[NameSpaces].apply("style"), "font-style-complex")
  val StyleJustifySingleWord: XmlAttribute = (implicitly[NameSpaces].apply("style"), "justify-single-word")
  val StyleAutoTextIndent: XmlAttribute = (implicitly[NameSpaces].apply("style"), "auto-text-indent")
  val StyleWritingMode: XmlAttribute = (implicitly[NameSpaces].apply("style"), "writing-mode")

  val FoFSize: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "font-size")
  val FoFWeight: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "font-weight")
  val FoFStyle: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "font-style")
  val FoColor: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "color")
  val FoBColor: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "background-color")
  val FoTextAlign: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "text-align")
  val FoLineHeight: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "line-height")
  val FoPageWidth: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "page-width")
  val FoMarginLeft: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "margin-left")
  val FoMarginRight: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "margin-right")
  val FoPaddingLeft: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "padding-left")
  val FoPaddingRight: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "padding-right")
  val FoParBreak: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "break-before")
  val FoParBreakAfter: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "break-after")
  val FoTextIndent: XmlAttribute = (implicitly[NameSpaces].apply("fo"), "text-indent")

}
