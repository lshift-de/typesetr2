package net.lshift.typesetr.parsers.odt

import net.lshift.typesetr.parsers.OdtParser
import net.lshift.typesetr.xml.XmlTag

object OdtTags {
  // main tags
  val Document: XmlTag = (OdtParser.ns("office"), "document-content")
  val AutomaticStyle: XmlTag = (OdtParser.ns("office"), "automatic-styles")
  val Font: XmlTag = (OdtParser.ns("office"), "font-face-decls")
  val Body: XmlTag = (OdtParser.ns("office"), "body")
  val Text: XmlTag = (OdtParser.ns("office"), "text")
  val MasterStyle: XmlTag = (OdtParser.ns("office"), "master-styles")
  val Scripts: XmlTag = (OdtParser.ns("office"), "scripts")
  val Annotation: XmlTag = (OdtParser.ns("office"), "annotation")
  val Styles: XmlTag = (OdtParser.ns("office"), "styles")

  val Frame: XmlTag = (OdtParser.ns("draw"), "frame")
  val Image: XmlTag = (OdtParser.ns("draw"), "image")

  val A: XmlTag = (OdtParser.ns("text"), "a")
  val BookmarkStart: XmlTag = (OdtParser.ns("text"), "bookmark-start")
  val BookmarkEnd: XmlTag = (OdtParser.ns("text"), "bookmark-end")
  val Bookmark: XmlTag = (OdtParser.ns("text"), "bookmark")
  val H: XmlTag = (OdtParser.ns("text"), "h")
  val List: XmlTag = (OdtParser.ns("text"), "list")
  val TextListItem: XmlTag = (OdtParser.ns("text"), "list-item")
  val TextListStyle: XmlTag = (OdtParser.ns("text"), "list-style")
  val Note: XmlTag = (OdtParser.ns("text"), "note")
  val NoteBody: XmlTag = (OdtParser.ns("text"), "note-body")
  val NoteCitation: XmlTag = (OdtParser.ns("text"), "note-citation")
  val P: XmlTag = (OdtParser.ns("text"), "p")
  val S: XmlTag = (OdtParser.ns("text"), "s")
  val Span: XmlTag = (OdtParser.ns("text"), "span")
  val Tab: XmlTag = (OdtParser.ns("text"), "tab")
  val C: XmlTag = (OdtParser.ns("text"), "c")
  val TextNameAttr: XmlTag = (OdtParser.ns("text"), "name")
  val Linebreak: XmlTag = (OdtParser.ns("text"), "line-break")
  val SeqDecl: XmlTag = (OdtParser.ns("text"), "sequence-decls")
  val StyleNameAttr: XmlTag = (OdtParser.ns("text"), "style-name")
  val SoftPageBreak: XmlTag = (OdtParser.ns("text"), "soft-page-break")
  val TextOutlineStyle: XmlTag = (OdtParser.ns("text"), "outline-style")
  val TextNotesConf: XmlTag = (OdtParser.ns("text"), "notes-configuration")
  val TextLineNumConf: XmlTag = (OdtParser.ns("text"), "linenumbering-configuration")

  val TableStyleNameAttr: XmlTag = (OdtParser.ns("table"), "style-name")
  val Table: XmlTag = (OdtParser.ns("table"), "table")
  val TableColumn: XmlTag = (OdtParser.ns("table"), "table-column")
  val TableRow: XmlTag = (OdtParser.ns("table"), "table-row")
  val TableCell: XmlTag = (OdtParser.ns("table"), "table-cell")

  val Id: XmlTag = (OdtParser.ns("xml"), "id")

  val HrefAttr: XmlTag = (OdtParser.ns("xlink"), "href")
  val HrefType: XmlTag = (OdtParser.ns("xlink"), "type")

  val Creator: XmlTag = (OdtParser.ns("dc"), "creator")

  // style tags
  val MasterPage: XmlTag = (OdtParser.ns("style"), "master-page")
  val StyleDefault: XmlTag = (OdtParser.ns("style"), "default-style")
  val StyleHeader: XmlTag = (OdtParser.ns("style"), "header")
  val StyleFooter: XmlTag = (OdtParser.ns("style"), "footer")
  val StyleStyle: XmlTag = (OdtParser.ns("style"), "style")
  val StyleName: XmlTag = (OdtParser.ns("style"), "name")
  val StyleFamily: XmlTag = (OdtParser.ns("style"), "family")
  val StylePProps: XmlTag = (OdtParser.ns("style"), "paragraph-properties")
  val StyleTProps: XmlTag = (OdtParser.ns("style"), "text-properties")
  val StylePageLayout: XmlTag = (OdtParser.ns("style"), "page-layout")
  val StylePageLayoutProps: XmlTag = (OdtParser.ns("style"), "page-layout-properties")

  val StyleParentStyle: XmlTag = (OdtParser.ns("style"), "parent-style-name")
  val StyleDisplayName: XmlTag = (OdtParser.ns("style"), "display-name")
  val StyleTableColProps: XmlTag = (OdtParser.ns("style"), "table-column-properties")
  val StyleTableRowProps: XmlTag = (OdtParser.ns("style"), "table-row-properties")
  val StyleTableCellProps: XmlTag = (OdtParser.ns("style"), "table-cell-properties")
  val StyleTableProps: XmlTag = (OdtParser.ns("style"), "table-properties")
  val StyleFFamilyName: XmlTag = (OdtParser.ns("style"), "font-name")
  val StyleTextUnderline: XmlTag = (OdtParser.ns("style"), "text-underline-style")
  val StyleTextLinethrough: XmlTag = (OdtParser.ns("style"), "text-line-through-style")
  val StyleTextPosition: XmlTag = (OdtParser.ns("style"), "text-position")
  val StyleColumnWidth: XmlTag = (OdtParser.ns("style"), "column-width")
  val StyleMinHeigh: XmlTag = (OdtParser.ns("style"), "min-row-height")

  val FoFSize: XmlTag = (OdtParser.ns("fo"), "font-size")
  val FoFWeight: XmlTag = (OdtParser.ns("fo"), "font-weight")
  val FoFStyle: XmlTag = (OdtParser.ns("fo"), "font-style")
  val FoColor: XmlTag = (OdtParser.ns("fo"), "color")
  val FoBColor: XmlTag = (OdtParser.ns("fo"), "background-color")
  val FoTextAlign: XmlTag = (OdtParser.ns("fo"), "text-align")
  val FoLineHeight: XmlTag = (OdtParser.ns("fo"), "line-height")
  val FoMarginLeft: XmlTag = (OdtParser.ns("fo"), "margin-left")
  val FoParBreak: XmlTag = (OdtParser.ns("fo"), "break-before")
  val FoTextIndent: XmlTag = (OdtParser.ns("fo"), "text-indent")

}
