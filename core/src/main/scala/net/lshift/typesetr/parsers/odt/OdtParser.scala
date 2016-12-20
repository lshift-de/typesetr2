package net.lshift.typesetr
package parsers

import net.lshift.typesetr.parsers.styles.{StyleId, DocumentStyle, StylePropKey, Style}
import odt.styles._
import net.lshift.typesetr.xml.attributes.{FontFamily, StyleAttribute}
import xml._
import odt._

import java.io.File

import scala.xml.{Elem, Atom, XML, Text}
import scalaz.Tags.First
import scalaz.{ Tags => STags, Tag => STag, _ }
import scalaz.Scalaz._
import shapeless.{Poly, Poly2, HList, HNil, :: => !:, Witness}

import util.{ValOfUnit, Logger}
import xml.{InternalTags => Tags}

import scala.language.{ postfixOps, implicitConversions, existentials }

class OdtParser() extends Parser {

  import OdtParser._

  type DocNode = scala.xml.Node

  private[this] var counter: Int = 1

  def parse(input: File)(implicit logger: Logger, config: cmd.Config): Either[String, ParsedDocument[DocNode]] = {

    logger.info(s"Parsing $input")

    val parsed = for {
      // 1. Take an input file, and attempt to unpack it
      //    since it is an ODT binary
      unpacked <- input.unpack().toRight("Cannot upack the .odt file").right
      root             <- unpacked._1.content.map(XML.loadFile).toRight("Cannot find a content.xml file").right // content.xml
      rootStyle        <- unpacked._1.style.map(XML.loadFile).toRight("Cannot find a styles.xml file").right // styles.xml

      // 2. Find all the key nodes in the unpacked binary (either in content.xml or styles.xml)
      // a) font node
      rawFont          <- (root \!! OdtTags.FontDecls).toRight("Cannot find fonts declarations").right
      // b) automatic style node in meta
      rawAutoStyle     <- (rootStyle \!! OdtTags.AutomaticStyle).toRight("Cannot find styles declarations in stylex.xml").right
      // c) body node in the content.xml
      rawBody          <- (root \!! OdtTags.Body).toRight("Cannot find a body of the document").right
      // d) automatic style node in the content.xml
      rawStyleInBody   <- (root \!! OdtTags.AutomaticStyle).toRight("Cannot find style declarations in the document").right

      //rawStyle <- rootStyle \!! OdtTags.Styles
      loadedStyles <- loadDocStyleFromMeta(rootStyle).toRight("Cannot infer basic meta information").right
      autoStyle        <- parseBody(rawAutoStyle)(loadedStyles._2, logger, implicitly[ParsingContext]).toRight("Failed to parse main style information").right

      styleInBody      <- parseBody(rawStyleInBody)(loadedStyles._2, logger, implicitly[ParsingContext]).toRight("Failed to parse document-specific style information").right
    } yield {
      val (styleNode, styleFromMeta) = loadedStyles
      // (optional) scripts node parsing
      val scriptsNode =
        (for {
          rawScripts <- root \!! OdtTags.Scripts
          scriptsNode <- parseBody(rawScripts)(styleFromMeta, logger, implicitly[ParsingContext])
        } yield scriptsNode :: Nil).getOrElse(List())

      val styleParser = OdtStyleParser.default
      val stylesFromDoc = styleParser.loadFromDocContent(root, styleFromMeta)
      val parsedChildrenNodes = rawBody.child.flatMap(parseBody(_)(stylesFromDoc, logger, implicitly[ParsingContext]))
      val body1 = Repr.makeElem(Tags.BODY, parsedChildrenNodes, contents = None, attrs = Nil)(rawBody, implicitly[NodeFactory.Aux[DocNode]])

      // The original nodes may be enriched with some new ones that
      // are added as part of the parsing process.
      // The new style nodes have to be included in the modified .odt binary.
      val reifiedStyleNodes = styleInBody.copy(styleInBody.body ++ newStyles.values)
      val reifiedStylesDict = newStyles.values.foldLeft(stylesFromDoc) {
        case (doc, styleNode) => styleParser.appendStyleNode(styleNode.source, doc) }

      if (!config.Ytmp)
        unpacked._2.deleteDirectory()

      (root,
        reifiedStylesDict,
        scriptsNode ::: (parseFonts(rawFont) :: reifiedStyleNodes :: body1 :: Nil))
    }

    parsed match {
      case Right((root, style, rootBody)) =>
        Right(ParsedDocument(root.wrap(tag = Tags.ROOT, body = rootBody), style))
      case Left(err) =>
        Left(err)
    }
  }

  // Just leave them as they are
  private def parseFonts(node: scala.xml.Node): Repr.Aux[DocNode] =
    node.wrapRec(tag = Tag.nodeTag)

  // FIXME:
  private var newStyles: Map[StyleId, Repr.Aux[scala.xml.Node]] = Map.empty


  def loadDocStyleFromMeta(node: scala.xml.Node)(implicit logger: Logger): Option[(Repr.Aux[DocNode], DocumentStyle.Aux[DocNode])] = {
    def length(prop: Option[String]): Option[Double] =
      prop.toRight("0cm").fold(ValOfUnit.parse, ValOfUnit.parse).map(_.toCm)

    val doc = (for {
      pgLayout   <- node \!! (OdtTags.AutomaticStyle / OdtTags.StylePageLayout / OdtTags.StylePageLayoutProps)
    } yield {
      for {
        marginLeft   <- length(pgLayout.attributes.getTag(OdtTags.FoMarginLeft))
        marginRight  <- length(pgLayout.attributes.getTag(OdtTags.FoMarginRight))
        paddingLeft  <- length(pgLayout.attributes.getTag(OdtTags.FoPaddingLeft))
        paddingRight <- length(pgLayout.attributes.getTag(OdtTags.FoPaddingRight))
        pageWidth    <- length(pgLayout.attributes.getTag(OdtTags.FoPageWidth))
      } yield {
        val w =
          pageWidth - (marginLeft + marginRight + paddingLeft + paddingRight)

        val rawHeader  = node \!! (OdtTags.MasterStyle / OdtTags.StyleMasterPage / OdtTags.StyleHeader)
        val rawFooter  = node \!! (OdtTags.MasterStyle / OdtTags.StyleMasterPage / OdtTags.StyleFooter)
        val headerOpt = rawHeader.flatMap(n => parseBody(n)(DocumentStyle.empty, implicitly[Logger], implicitly[ParsingContext]))
        val footerOpt = rawFooter.flatMap(n => parseBody(n)(DocumentStyle.empty, implicitly[Logger], implicitly[ParsingContext]))


        val emptyStyleSheet = DocumentStyle(headerOpt, footerOpt, w centimeters)

        val docWithStyles = OdtStyleParser.default.loadFromDocContent(node, emptyStyleSheet)

        logger.debug(s"Loaded style:\n${docWithStyles}")

        docWithStyles
      }
    }).flatten

    val node1 = node.wrapRec(Tag.nodeTag)

    doc map ((node1, _))
  }

  def parseBody(node: scala.xml.Node)(implicit docStyle: DocumentStyle.Aux[DocNode], logger: Logger, ctx: ParsingContext): Option[Repr.Aux[DocNode]] = {
    node match {
      case Text(text) =>
        Some(node.wrap(tag = Tag.textTag, body = Nil, contents = Some(text)))
      case _ =>
        parseBodyElement(node)
    }
  }

  private def parseBodyElement(node: DocNode)(implicit docStyle: DocumentStyle.Aux[DocNode], logger: Logger, ctx: ParsingContext): Option[Repr.Aux[DocNode]] = {
    // TODO: handle lists

    implicit def toOpt[T](x: Repr.Aux[T]): Option[Repr.Aux[T]] =
      Some(x)

    lazy val children = node.child.flatMap(parseBody)
    implicit val source: DocNode = node

    lazy val sty: Style = {
      val styleIdOpt = OdtStyleId.forNonStyleNode(node)
      (for {
        styleId <- styleIdOpt
        style   <- docStyle.style(styleId)
      } yield style).getOrElse {
        logger.info(s"no style for ${styleIdOpt}")
        OdtStyle.empty
      }
    }

    node.xmlTag match {
      case OdtTags.Tab =>
        val tabsNum = node.attributes.getTag(OdtTags.C).map(_.toInt).getOrElse(1)
        val tabNodes =
          Repr.makeTextElem[DocNode](tabEncoded * tabsNum, synthetic = true)

        Repr.makeElem(Tag.nodeTag, tabNodes +: children, contents = None, attrs = Nil)

      case OdtTags.S =>
        val spaces =
          node.attributes.getTag(OdtTags.C).map(_.toInt).getOrElse(1)

        val whitespaceNodes =
          Repr.makeTextElem[DocNode](spaceEncoded * spaces, synthetic = true)

        Repr.makeElem(
          Tag.nodeTag,
          whitespaceNodes +: children,
          contents = Some(spaceEncoded*spaces),
          attrs = Attribute(InternalAttributes.indent, spaces.toString) :: Nil)

      case OdtTags.Linebreak =>
        Repr.makeTextElem[DocNode](linebreakEncoded, synthetic = true)

      case OdtTags.H =>
        logger.debug(s"[parser] header tag: $node")
        for {
          styleTpe <- sty.tpe if !(node isBlank)
        } yield {
          val modifiers = styleTpe match {
            case hStyle: styles.HeadingStyle =>
              Attribute(InternalAttributes.outlineLvl, hStyle.index.toString) :: Nil
            case _ =>
              Nil
          }
          node.wrap(tag = styleTpe.tag, body = children, attributes = modifiers)
        }

      case OdtTags.TextList =>
        val listStyle = docStyle.newListLevelContext
        val children = node.child.flatMap(parseBody(_)(listStyle, logger, ctx))
        Repr.makeElem(tag = Tags.LIST, body = children, contents = None, attrs = Nil)

      case OdtTags.TextListItem =>
        node.wrap(tag = Tags.LI, body = children)

      case OdtTags.Annotation =>
        node.wrap(tag = Tags.ASIDE, body = children)

      case OdtTags.Creator | OdtTags.NoteCitation | OdtTags.BookmarkEnd =>
        // TODO: include the potential child nodes
        None

      case t @ OdtTags.Note =>
        Repr.makeElem(tag = Tags.FOOTNOTE, body = children, contents = None, attrs = Nil)

      case OdtTags.NoteBody =>
        // TODO: postprocessing should remove any whitespaces
        Repr.makeElem(Tag.nodeTag, children, contents = None, attrs = Nil)

      case OdtTags.P =>
        // infer indentation level from the style
        val indentLvl =
          First(sty.marginLeft) |+| First(sty.textIndent)

        val headerLevel = sty.parent.flatMap(p => HeadingP.findFirstMatchIn(p.name)).map(_.group("level"))
        val headerElem = headerLevel.map { (lvl: String) =>
          val headerSource = new Elem(
          prefix = OdtTags.H.namespace.short.value,
          label = OdtTags.H.tag,
          attributes1 = source.attributes,
          minimizeEmpty = source.asInstanceOf[Elem].minimizeEmpty,
          scope = source.scope,
          child = (source.child: _*))
          Repr.makeElem(tag = Tags.HEADER, body = children, contents = None,
            attrs = Attribute(InternalAttributes.outlineLvl, lvl) :: Nil)(
            headerSource, implicitly[NodeFactory.Aux[DocNode]])
        }

        val indentElem =  scalaz.Tag.unwrap(indentLvl).filter(_.toCm > 1).map { lvl =>
            // Augment the style of the paragraph to
            // introduce blockquote.

            // Note: we only add a new style here, but do not modify the node
            // directly here.
            // The change of style is reflected in the Typesetr's internal
            // attribute list that carries over the new style info name.
            // The latter will be modified, if necessary, in the Odt writer.
            val (newStyleId, fact) = OdtStyleFactory.quotingStyle(parent = sty, counter)
            newStyles = if (newStyles.contains(newStyleId))
              newStyles
            else
              newStyles + (newStyleId -> fact.create(newStyleId))

            val attr1 = Attribute(InternalAttributes.style, newStyleId.name) :: Nil
            val children1 = node.child.flatMap(parseBody(_)(docStyle, logger, ctx.copy(inBlock = true)))
            Repr.makeElem(tag = Tags.BLOCK, children1, attrs = attr1, contents = None)(
              source, implicitly[NodeFactory.Aux[DocNode]])
          }

        val fallback = Some(Repr.makeElem(tag = Tags.P, body = children, attrs = Nil, contents = None))

        scalaz.Tag.unwrap(First(headerElem) |+| First(indentElem) |+| First(fallback))

      case OdtTags.Span =>
        // Translate attributes into individual, nested nodes
        lazy val body1 = translateStyleToTags(children, styleToTagsMap, sty)

        // Note: Here we dictate how the code block (potentially) will be displayed.
        //       Spaces have to be preserved in the code blocks, and they inherit
        //       the indentation from their first child that has that info, if any.
        val attrs2 = children.flatMap(_.getAttribute(InternalAttributes.indent)).headOption.map(_ :: Nil).getOrElse(Nil)
        val body2 = sty.fontFamily.filter(_.isCodeFont).map { _ =>
          val styleName = if (ctx.inBlock) "Standard" else {
            val (newStyleId, fact) = OdtStyleFactory.inlineCodeStyle(parent = sty)
            newStyles =
              if (newStyles.contains(newStyleId)) newStyles
              else newStyles + (newStyleId -> fact.create(newStyleId))

            newStyleId.name
          }
          Repr.makeElem(
            tag = if (ctx.inBlock) Tags.BLOCKCODE else Tags.CODE,
            body = children,
            contents = None,
            // Style name is reset, so that Pandoc does not attempt to perform
            // its own code formatting
            attrs = Attribute(InternalAttributes.style, styleName) :: attrs2)
        }

        // Maybe it is a caption
        val txtContent = children.flatMap(_.extractPlainText(deep = false)).mkString
        val body3 = if (txtContent == CAPTION_TXT) {
          if (docStyle.style(OdtStyleFactory.CaptionStyleNameAndFamily.swap).isEmpty) {
            val (newStyleId, fact) = OdtStyleFactory.tableCaptionStyle(parent = sty)
            // Caption style needs to be present in the situation we
            // later want to associate it with a particular node.
            // This is required by Pandoc.
            // i.e. we cannot have references to non-existent styles
            // even if we only check their names.

            // avoid duplicates
            if (!newStyles.contains(newStyleId))
              newStyles = newStyles + (newStyleId -> fact.create(newStyleId))
          }
          Some(
            Repr.makeElem(
              tag = Tags.CAPTION,
              body = children,
              contents = None,
              attrs = Nil
            )
          )
        } else None

        scalaz.Tag.unwrap(
          First(body2) |+| First(body3) |+|
            First(Repr.makeElem(tag = Tags.SPAN, body = body1, contents = None, attrs = Nil)))

      case OdtTags.A =>
        val hrefAttr = AttributeKey(OdtTags.HrefType)
        // TODO: re-enable the assert
        //assert((tpeAttr inAttributes (attr)).getOrElse("") == "simple")
        // TODO: store a definition separately

        val hrefLink = source.attributes.getTag(OdtTags.HrefAttr)
        val attributes = hrefLink.map(link => Attribute(InternalAttributes.href, link) :: Nil).getOrElse(Nil)
        val body = children.flatMap(child =>
          whack(child, _ hasTag (Tags.SPAN | Tags.U)))
        Repr.makeElem(tag = Tags.A, body, contents = None, attrs = attributes)

      case OdtTags.BookmarkStart | OdtTags.Bookmark =>
        // TOOD: Missing guards
        val attrs = node.attributes.getTag(OdtTags.TextNameAttr).
          map(v => Attribute(InternalAttributes.href, v) :: Nil).getOrElse(Nil)
        node.wrap(tag = Tags.LABEL, body = children, attributes = attrs)

      case OdtTags.Table =>
        logger.warn(s"[limitation] Ignoring Table node")
        // TODO: ignore for the moment
        val children1 = node.child.flatMap(parseBody(_)(docStyle, logger, ctx.copy(inTable = true)))
        node.wrap(tag = Tags.TABLE, body = children1, attributes = Nil)

      case OdtTags.TableRow =>
        node.wrap(tag = Tags.TR, body = children, attributes = Nil)

      case OdtTags.TableCell =>
        node.wrap(tag = Tags.TD, body = children, attributes = Nil)

      case OdtTags.TableColumn =>
        node.wrap(tag = Tags.TH, body = children, attributes = Nil)

      case t@ OdtTags.Frame =>
        val width = source.attributes.getTag(OdtTags.SvgWidth).toRight("0cm").fold(ValOfUnit.parse, ValOfUnit.parse)
        val relWidth = width.map (v => (v.toCm / docStyle.textWidth.toCm)) getOrElse(0.0)

        // If the frame has a caption, we only modify the width of the figure
        // but leave the formatting to Pandoc. Pandoc is too sensitive
        // to the structure of the document and with our contributions it seems
        // to handle it correctly.
        val children1 = node.child.flatMap(parseBody(_)(docStyle, logger, ctx.copy(inFrame = true)))
        val noTypesetrTransform = !ctx.inFrame && children1.exists(_.tag == Tags.IMG)

        val frameAttributes = source.attributes.getTag(OdtTags.AnchorTpe).map { v =>
          Attribute(InternalAttributes.imgWidth,
                    Utils.inferWidthPercentage(relWidth)) :: (
            if (noTypesetrTransform)
              Attribute(InternalAttributes.frameDisplay,
                        Utils.shouldInline((v == "as-char") && !ctx.inTable, relWidth)) :: Nil
            else Nil)
        }

        node.wrap(tag = Tags.FRAME, body = children1, attributes = frameAttributes.getOrElse(Nil))

      case OdtTags.Image =>

        val attr =
          node.attributes.getTag(OdtTags.HrefAttr) map { v =>
            Attribute(InternalAttributes.src, v) :: Nil
          } getOrElse (Nil)

        node.wrap(tag = Tags.IMG, body = children, attributes = attr)

      case t @ OdtTags.StyleHeader =>
        node.wrap(tag = t.toInternalTag, body = children)

      case t @ OdtTags.StyleFooter =>
        node.wrap(tag = t.toInternalTag, body = children)

      // The main text node in odt
      case t @ OdtTags.Text =>
        node.wrap(tag = t.toInternalTag, body = children)

      case t @ OdtTags.SoftPageBreak =>
        node.wrap(tag = t.toInternalTag, body = children)

      case t @ OdtTags.TextBox =>
        node.wrap(tag = t.toInternalTag, body = children)

      case OdtTags.SeqDecl =>
        None

      case t @ OdtTags.Scripts =>
        node.wrap(tag = t.toInternalTag, body = children)

      case t @ OdtTags.AutomaticStyle =>
        parseStyleNode(node)

      case t @ OdtTags.Seq =>
        None

      case t @ OdtTags.TextListHeader =>
        node.wrap(tag = t.toInternalTag, body = children)

      case t @ OdtTags.TextPageNumber =>
        node.wrap(tag = t.toInternalTag, body = children)

      case tag =>
        // TODO:
        // if sty and sty.type.endswith('title'):
        logger.info(s"Ignoring $tag in node: ${node}")
        ???
    }
  }

  // TODO: avoid double pass-through
  // Currently just pass-through
  private def parseStyleNode(node: scala.xml.Node)(implicit sty: DocumentStyle.Aux[DocNode], logger: Logger): Option[Repr.Aux[DocNode]] = {
    lazy val children = node.child.flatMap(parseStyleNode(_))
    node.xmlTag match {
      // Small subset, currently just pass
      /*
      case t @ OdtTags.StyleStyle =>
      case t @ OdtTags.StylePProps =>
      case t @ OdtTags.StyleTProps =>
      case t @ OdtTags.StylePageLayout =>
      case t @ OdtTags.StylePageLayoutProps =>
      case t @ OdtTags.AutomaticStyle =>*/
      case t =>
        Some(node.wrap(tag = t.toInternalTag, body = children))
    }
  }

  /*
   * Translate `span` nodes into the internal representation
   * based on the style of the particular node.
   *
   * Note: this may translate a single `<span>` node into a
   * cascade of nested nodes.
   */
  private def translateStyleToTags(body: Seq[Repr.Aux[DocNode]],
                                   trans: StyleToTags, sty: Style)(
                                     implicit orig: scala.xml.Node): Seq[Repr.Aux[DocNode]] = {

    class translate(style: Style, orig: scala.xml.Node) extends Poly2 {
      implicit def styletoTagToXml[T <: StylePropKey.Of] =
        at[Seq[Repr.Aux[DocNode]], StyleToTag[T]] { (acc, sty2Tag) =>
          style.unsafeProperty(sty2Tag.styleKey).flatMap(propValue =>
            for {
              val2Tag <- sty2Tag.allowedValues.find(_.value == propValue)
              attributeTag <- sty2Tag.styleKey.name
            } yield Seq(Repr.makeElem(tag = val2Tag.tag, body = acc, contents = None, attrs = Nil)(orig, implicitly[NodeFactory.Aux[DocNode]]))
          ).getOrElse(acc)
        }
    }

    object translate {
      def apply(sty: Style, node: scala.xml.Node) = new translate(sty, node)
    }

    val translateForStyle = translate(sty, orig)
    import translateForStyle._

    trans.foldLeft(body)(translateForStyle)
  }

  implicit lazy val nodeConfig: NodeConfigs.WithNode[scala.xml.Node] =
    new OdtNodeConfig

}

/**
  * Object represetning the content (child-parent) relationship
  * between the nodes, during parsing
  *
  * @param inFrame is the parser within a frame node
  * @param inTable is the parser currently parsing a table
  * @param inBlock is the parser currently parsing some block
  */
case class ParsingContext(
                           inFrame: Boolean,
                           inTable: Boolean,
                           inBlock: Boolean)
object ParsingContext {
  implicit def emptyCtx: ParsingContext = ParsingContext(inFrame = false, inTable = false, inBlock = false)
}

object OdtParser {

  // TODO: replace those abstractions with HList of Records.
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

  type StyleToTags =
    StyleToTag[OdtStylePropKeys.Underline.type] !:
      StyleToTag[OdtStylePropKeys.FontWeight.type] !:
        StyleToTag[OdtStylePropKeys.FontStyleProp.type] !:
          StyleToTag[OdtStylePropKeys.LineThrough.type] !:
            StyleToTag[OdtStylePropKeys.TextPosition.type] !:
              HNil

  private val styleToTagsMap: StyleToTags =
    StyleToTag(OdtStylePropKeys.Underline)(ValToTag[attributes.Underline](attributes.Underline.Solid, Tags.U) :: Nil) ::
      StyleToTag(OdtStylePropKeys.FontWeight)(ValToTag[attributes.FontWeight](attributes.FontWeight.Bold, Tags.B) :: Nil) ::
        StyleToTag(OdtStylePropKeys.FontStyleProp)(ValToTag[attributes.FontStyle](attributes.FontStyle.Italic, Tags.I) :: Nil) ::
          StyleToTag(OdtStylePropKeys.LineThrough)(ValToTag[attributes.LineThrough](attributes.LineThrough.Solid, Tags.S) :: Nil) ::
            StyleToTag[OdtStylePropKeys.TextPosition.type](OdtStylePropKeys.TextPosition)(ValToTag(attributes.TextPosition.Sub, Tags.SUB) :: ValToTag(attributes.TextPosition.Sup, Tags.SUP) :: Nil) ::
              HNil

  private final lazy val spaceEncoded = " &nbsp;"
  private final lazy val linebreakEncoded = "br"
  private final lazy val tabEncoded = " \\t"
  private final lazy val HeadingP = new scala.util.matching.Regex("Heading_20_(\\d+)", "level")

  private final val CAPTION_TXT = "Caption:"

}
