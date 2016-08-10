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
  val ListItem: XmlTag = (OdtParser.ns("text"), "list-item")
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
  val StylePProps: XmlTag = (OdtParser.ns("style"), "paragraph-properties")
  val StyleTProps: XmlTag = (OdtParser.ns("style"), "text-properties")
  val StylePageLayout: XmlTag = (OdtParser.ns("style"), "page-layout")
  val StylePageLayoutProps: XmlTag = (OdtParser.ns("style"), "page-layout-properties")

}
