package net.lshift.typesetr
package parsers

import net.lshift.typesetr.parsers.Repr.Aux
import net.lshift.typesetr.parsers.odt.styles.{StylePropKey, StyleFactory, Style}
import net.lshift.typesetr.xml.attributes.StyleAttribute
import xml._
import net.lshift.typesetr.parsers.odt._

import java.io.File

import scala.xml.{ Atom, XML, Text }

import scalaz.Tags.First
import scalaz.{ Tags => STags, Tag => STag, _ }
import scalaz.Scalaz._

import util.Logger

import scala.language.{ postfixOps, implicitConversions }

import shapeless.{Poly, Poly2, HList, HNil, :: => !:, Witness}

class OdtParser() extends Parser {

  import OdtParser._

  type Underlying = scala.xml.Node

  lazy val wrapper = implicitly[NodeRepr[Underlying]]

  def parseToRawBody(input: File,
                     rewrittenInput: Boolean,
                     makeTransclusions: Boolean)(implicit logger: Logger): Repr.Aux[Underlying] = {

    logger.info(s"Parsing $input")

    val parsed = for {
      inFile <- input.unpack()
      root <- inFile.content.map(XML.loadFile)
      rootStyle <- inFile.style.map(XML.loadFile)

      rawFont <- rootStyle \!! OdtTags.Font
      rawAutoStyle <- rootStyle \!! OdtTags.AutomaticStyle
      rawStyle <- rootStyle \!! OdtTags.Styles
      (styleNode, style) <- loadStyles(rootStyle)
      autoStyle <- parseBody(rawAutoStyle)(style, logger)

      rawBody <- root \!! OdtTags.Body
      rawScripts <- root \!! OdtTags.Scripts
      scriptsNode <- parseBody(rawScripts)(style, logger)
      rawStyleInBody <- root \!! OdtTags.AutomaticStyle
      styleInBody <- parseBody(rawStyleInBody)(style, logger)
    } yield {
      val bodyNodes = rawBody.child.flatMap(parseBody(_)(style, logger))
      val body1 = Repr.makeElem(Tags.BODY, bodyNodes)(rawBody, wrapper)

      (root, scriptsNode :: parseFonts(rawFont) :: styleInBody :: body1 :: Nil)
    }

    parsed match {
      case Some((root, rootBody)) =>
        root.wrap(tag = Tags.ROOT, body = rootBody)
      case None =>
        ???
    }
  }

  def rewriteInput(meta: Any, unaugmentedMeta: Any, transclusions: Any, asides: Any, rewriteInfo: Any): Any =
    ???

  // Just leave them as they are
  private def parseFonts(node: scala.xml.Node): Repr.Aux[Underlying] =
    node.wrapRec(tag = Tag.nodeTag)

  private def inCm(v: String): Option[Int] =
    sizeP.findFirstMatchIn(v) match {
      case Some(sizeP(size)) => Some(size.toInt)
      case res               => None
    }

  private def layoutData(meta: scala.xml.MetaData, attr: AttributeKey, owner: scala.xml.Node): Option[Int] = {
    val tag: XmlTag = (OdtParser.ns("fo"), attr.key)
    meta.getTag(tag).toRight("0cm").fold(inCm, inCm)
  }

  def loadStyles(node: scala.xml.Node)(implicit logger: Logger): Option[(Repr.Aux[Underlying], DocumentStyle.Aux[Underlying])] = {
    val loader = StyleFactory()
    val doc = (for {
      rawHeader <- node \!! (OdtTags.MasterStyle / OdtTags.MasterPage / OdtTags.StyleHeader)
      rawFooter <- node \!! (OdtTags.MasterStyle / OdtTags.MasterPage / OdtTags.StyleFooter)
      pageLayout <- node \!! (OdtTags.AutomaticStyle / OdtTags.StylePageLayout)
    } yield {
      for {
        pageWidth <- layoutData(pageLayout.attributes, "page-width", pageLayout)
        marginLeft <- layoutData(pageLayout.attributes, "margin-left", pageLayout)
        marginRight <- layoutData(pageLayout.attributes, "margin-right", pageLayout)
        paddingLeft <- layoutData(pageLayout.attributes, "padding-left", pageLayout)
        paddingRight <- layoutData(pageLayout.attributes, "padding-right", pageLayout)
        header <- parseBody(rawHeader)(DocumentStyle.empty, implicitly[Logger])
        footer <- parseBody(rawFooter)(DocumentStyle.empty, implicitly[Logger])
      } yield {
        val w =
          pageWidth - (marginLeft + marginRight +
            paddingLeft + paddingRight)

        val doc = DocumentStyle(header, footer, w)

        val doc1 = loader.loadFromStyleDoc(node, doc)

        logger.debug("Loaded style:")
        logger.debug(doc1.toString)

        doc1
      }
    }).flatten

    val node1 = node.wrapRec(Tag.nodeTag)

    doc map ((node1, _))
  }

  def parseBody(node: scala.xml.Node)(implicit docStyle: DocumentStyle.Aux[Underlying], logger: Logger): Option[Repr.Aux[Underlying]] =
    node match {
      case Text(text) =>
        Some(node.wrap(tag = Tag.textTag, body = Nil, contents = Some(text)))
      case _: Atom[_] =>
        ???
      case _ =>
        parseBodyElement(node, Nil)
    }

  private def parseBodyElement(node: Underlying, attr: List[Attribute])(implicit docStyle: DocumentStyle.Aux[Underlying], logger: Logger): Option[Repr.Aux[Underlying]] = {
    // TODO: handle page breaks
    // TODO: handle lists

    implicit def toOpt[T](x: Repr.Aux[T]): Option[Repr.Aux[T]] =
      Some(x)

    lazy val children = node.child.flatMap(parseBody)
    implicit val source: Underlying = node

    val sty: Style = Style.empty // todo: need to extract from doc styles
    // based on the context name

    node.xmlTag match {
      case OdtTags.Tab =>
        val tabsNum = node.attributes.getTag(OdtTags.C).map(_.toInt).getOrElse(1)
        val tabNodes = Repr.makeTextElem[Underlying](" \t" * tabsNum, synthetic = true)

        Repr.makeElem(Tag.nodeTag, tabNodes +: children)

      case OdtTags.S =>
        val spaces =
          node.attributes.getTag(OdtTags.C).map(_.toInt).getOrElse(1)

        val whitespaceNodes = Repr.makeTextElem[Underlying](" &nbsp;" * spaces, synthetic = true)
        Repr.makeElem(Tag.nodeTag, whitespaceNodes +: children)

      case OdtTags.Linebreak =>
        Repr.makeTextElem[Underlying]("br", synthetic = true)

      case OdtTags.H =>
        // TODO: why on non-blank we wrap it?
        logger.debug(s"[parser] header tag: $node")
        val repr = for {
          tagName <- sty.tpe if !(node isBlank)
        } yield node.wrap(tag = ???, body = children)

        repr

      case OdtTags.List =>
        // attr:
        // - start
        // - data-continue-list

        // TODO: FIX
        logger.warn(s"Ignoring List")
        None

      case OdtTags.TextListItem =>
        node.wrap(tag = xml.Tags.LI, body = children)

      case OdtTags.Annotation =>
        node.wrap(tag = xml.Tags.ASIDE, body = children)

      case OdtTags.Creator | OdtTags.NoteCitation | OdtTags.BookmarkEnd =>
        // TODO: include the potential child nodes
        None

      case t @ OdtTags.Note =>
        // TODO: bring back safe-checks
        children match {
          case node :: Nil =>
            //children
            node
          case _ =>
            None
        }

      case OdtTags.NoteBody =>
        // TODO: postprocessing should get rid of the whitespaces
        Repr.makeElem(xml.Tags.FOOTNOTE, children)

      case OdtTags.P =>
        // infer indentation level from the style
        val indentLvl =
          First(sty.marginLeft) |+| First(sty.textIndent)

        Some(scalaz.Tag.unwrap(indentLvl) map { lvl =>
          val attr1 = Attribute("indent", lvl.toString) :: attr
          Repr.makeElem(tag = xml.Tags.BLOCK, children, attr1)
        } getOrElse (node.wrap(tag = xml.Tags.P, body = children)))

      case OdtTags.Span =>
        // TODO: Style handling
        //println(node.child)
        val body1 = translateStyleToTags(children, styleToTagsMap, sty)
        sty.fontFamily.foldLeft(body1) {
          case (b, font) =>
            if (Utils.isCodeFont(font)) Repr.makeElem(Tags.CODE, b) :: Nil
            else b
        }
        val body2 = sty.fontFamily.map(font =>
          if (Utils.isCodeFont(font)) Repr.makeElem(Tags.CODE, body1) :: Nil
          else body1).getOrElse(body1)

        Repr.makeElem(tag = xml.Tags.SPAN, body = body2)

      case OdtTags.A =>
        val tpeAttr = AttributeKey(OdtTags.HrefType)
        // TODO: re-enable the assert
        //assert((tpeAttr inAttributes (attr)).getOrElse("") == "simple")

        val body = children.flatMap(child =>
          whack(child, _ hasTag (xml.Tags.SPAN | xml.Tags.U)))
        Repr.makeElem(tag = xml.Tags.A, body)

      case OdtTags.BookmarkStart =>
        // TOOD: Missing guards
        val attr: List[Attribute] = node.attributes.getTag(OdtTags.TextNameAttr).
          map(v => Attribute("href", v) :: Nil).getOrElse(Nil)
        node.wrap(tag = xml.Tags.A, body = children, attributes = attr)

      case OdtTags.Table =>
        logger.warn(s"[limitation] Ignoring Table node")
        // TODO: ignore for the moment
        None

      case OdtTags.TableRow =>
        node.wrap(tag = xml.Tags.TR, body = children)

      case OdtTags.TableCell =>
        node.wrap(tag = xml.Tags.TD, body = children)

      case OdtTags.TableColumn =>
        node.wrap(tag = xml.Tags.COL, body = children)

      case OdtTags.Frame =>
        // TODO: ignore for the moment
        logger.warn(s"Ignoring Frame node")
        //Utils.makeFigure[Underlying](???,
        //  ???, children, FigureInfo.fromNode(node))
        None

      case OdtTags.Image =>

        val attr =
          node.attributes.getTag(OdtTags.HrefAttr) map { v =>
            Attribute("src", v) :: Nil
          } getOrElse (Nil)

        node.wrap(tag = xml.Tags.IMG, body = children, attributes = attr)

      case t @ OdtTags.StyleHeader =>
        node.wrap(tag = t.toInternalTag, body = children)

      case t @ OdtTags.StyleFooter =>
        node.wrap(tag = t.toInternalTag, body = children)

      // The main text node in odt
      case t @ OdtTags.Text =>
        node.wrap(tag = t.toInternalTag, body = children)

      case OdtTags.SeqDecl | OdtTags.Bookmark | OdtTags.SoftPageBreak =>
        None

      case t @ OdtTags.Scripts =>
        node.wrap(tag = t.toInternalTag, body = children)

      case t @ OdtTags.AutomaticStyle =>
        parseStyleNode(node)

      case tag =>
        // TODO:
        // if sty and sty.type.endswith('title'):
        logger.info(s"Ignoring $tag in node: ${node}")
        ???
    }
  }

  // TODO: Build style-map
  // Currently just pass-through
  private def parseStyleNode(node: scala.xml.Node)(implicit sty: DocumentStyle.Aux[Underlying], logger: Logger): Option[Repr.Aux[Underlying]] = {
    lazy val children = node.child.flatMap(parseStyleNode(_))
    node.xmlTag match {
      case t @ OdtTags.StyleStyle =>
        Some(node.wrap(tag = t.toInternalTag, body = children))
      case t @ OdtTags.StylePProps =>
        Some(node.wrap(tag = t.toInternalTag, body = children))
      case t @ OdtTags.StyleTProps =>
        Some(node.wrap(tag = t.toInternalTag, body = children))
      case t @ OdtTags.StylePageLayout =>
        Some(node.wrap(tag = t.toInternalTag, body = children))
      case t @ OdtTags.StylePageLayoutProps =>
        Some(node.wrap(tag = t.toInternalTag, body = children))
      case t @ OdtTags.AutomaticStyle =>
        Some(node.wrap(tag = t.toInternalTag, body = children))
      case t =>
        None
    }
  }

  /*
   * Translate `span` nodes into the internal representation
   * based on the style of the particular node.
   *
   * Note: this may translate a single `<span>` node into a
   * nested
   */
  private def translateStyleToTags(body: Seq[Repr.Aux[Underlying]],
                                   trans: StyleToTags, sty: Style)(
                                     implicit orig: scala.xml.Node): Seq[Repr.Aux[Underlying]] = {

    class translate(style: Style, orig: scala.xml.Node) extends Poly2 {
      implicit def styletoTagToXml[T <: StylePropKey.Of] =
        at[Seq[Repr.Aux[Underlying]], StyleToTag[T]] { (acc, sty2Tag) =>
          style.unsafeProperty(sty2Tag.styleKey).flatMap(propValue =>
            for {
              val2Tag <- sty2Tag.allowedValues.find(_.value == propValue)
              attributeTag <- sty2Tag.styleKey.name
              src <- orig.withAttribute(Attribute(attributeTag, val2Tag.value.name), acc)
            } yield Repr.makeElem(tag = val2Tag.tag, body = acc)(src, wrapper)
          ).getOrElse(acc);
          acc
        }
    }

    object translate {
      def apply(sty: Style, node: scala.xml.Node) = new translate(sty, node)
    }

    import shapeless.ops.hlist.LeftFolder.{ hlistLeftFolder => folder }

    //import translate._
    val translateForStyle = translate(sty, orig)
    import translateForStyle._

    // FIXME: do we really need to help the inferencer by
    // providing explicitly some implicit arguments?
    trans.foldLeft(body)(translateForStyle)(
      // Boilerplate to help the compiler with types
      folder(translateForStyle.styletoTagToXml, folder))
  }

}

object OdtParser {
  abstract class StyleToTag[T <: StylePropKey.Of] {
    val styleKey: T
    val witness: Witness.Aux[T]
    val allowedValues: List[ValToTag[styleKey.Result]]
  }

  object StyleToTag {
    def apply[T <: StylePropKey.Of](key0: T)(vs: List[ValToTag[key0.Result]])(implicit w: Witness.Aux[T]): StyleToTag[T] =
      new StyleToTag[T] {
        val styleKey: key0.type = key0
        val witness: Witness.Aux[T] = w
        val allowedValues: List[ValToTag[styleKey.Result]] = vs
      }
  }

  case class ValToTag[+T <: StyleAttribute](value: T, tag: Tag)

  val ns: NameSpaces = OdtNameSpaces(Map(
    "style" -> "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
    "text" -> "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
    "office" -> "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
    "draw" -> "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
    "table" -> "urn:oasis:names:tc:opendocument:xmlns:table:1.0",

    // openoffice crudified standard namespaces
    "fo" -> "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
    "svg" -> "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",

    // general stands
    "dc" -> "http://purl.org/dc/elements/1.1/",
    "xlink" -> "http://www.w3.org/1999/xlink",
    "xml" -> "http://www.w3.org/XML/1998/namespace"))

  private implicit val defaultNS: NameSpaces = ns

  private implicit lazy val defaultWrapper: NodeRepr[scala.xml.Node] =
    new OdtNodeRepr

  private class OdtNodeRepr extends NodeRepr[scala.xml.Node] {

    def create(tag: Tag,
               elem: scala.xml.Node,
               children: Seq[Aux[scala.xml.Node]] = Nil,
               attrs: List[Attribute] = Nil,
               contents: Option[String] = None): Aux[scala.xml.Node] =
      OdtRepr(elem, children, tag, contents, attrs)

    def createWithAttributes(
      tag: Tag,
      elem: scala.xml.Node,
      children: Seq[Aux[scala.xml.Node]],
      attrs: List[Attribute]): Aux[scala.xml.Node] =
      OdtRepr(elem, children, tag, None, attrs)

    def createWithContents(tag: Tag,
                           elem: scala.xml.Node,
                           contents: String): Aux[scala.xml.Node] =
      OdtRepr(elem, Nil, tag, Some(contents), Nil)

    def textNode(text: String): scala.xml.Node =
      Text(text)
  }

  private case class OdtRepr(
    source: scala.xml.Node,
    body: Seq[Aux[scala.xml.Node]],
    tag: Tag,
    contents: Option[String],
    attr: List[Attribute]) extends Repr {
    if (contents.isEmpty && (tag == Tag.textTag)) ???
    type R = scala.xml.Node
    type BodyTpe = Repr.Aux[scala.xml.Node]
  }

  type StyleToTags =
    StyleToTag[StylePropKey.Underline.type] !:
      StyleToTag[StylePropKey.FontWeight.type] !:
        StyleToTag[StylePropKey.FontStyleProp.type] !:
          StyleToTag[StylePropKey.LineThrough.type] !:
            StyleToTag[StylePropKey.TextPosition.type] !:
              HNil

  private val styleToTagsMap: StyleToTags =
    StyleToTag(StylePropKey.Underline)(ValToTag[attributes.Underline](attributes.Underline.Solid, xml.Tags.U) :: Nil) ::
      StyleToTag(StylePropKey.FontWeight)(ValToTag[attributes.FontWeight](attributes.FontWeight.Bold, xml.Tags.B) :: Nil) ::
        StyleToTag(StylePropKey.FontStyleProp)(ValToTag[attributes.FontStyle](attributes.FontStyle.Italic, xml.Tags.I) :: Nil) ::
          StyleToTag(StylePropKey.LineThrough)(ValToTag[attributes.LineThrough](attributes.LineThrough.Solid, xml.Tags.S) :: Nil) ::
            StyleToTag[StylePropKey.TextPosition.type](StylePropKey.TextPosition)(ValToTag(attributes.TextPosition.Sub, xml.Tags.SUB) :: ValToTag(attributes.TextPosition.Sup, xml.Tags.SUP) :: Nil) ::
              HNil

  private final val sizeP = """(\d+)cm""".r
}
