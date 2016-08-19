package net.lshift.typesetr
package parsers

import net.lshift.typesetr.parsers.Repr.Aux
import net.lshift.typesetr.parsers.odt.styles.{StylePropKey, StyleFactory, Style}
import net.lshift.typesetr.xml.attributes.StyleAttribute
import odt.styles.StyleId
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

import xml.{InternalTags => Tags}

class OdtParser() extends Parser {

  import OdtParser._

  type Underlying = scala.xml.Node

  lazy val wrapper = implicitly[NodeFactory[Underlying]]

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
      (styleNode, style) <- loadDocStyleFromMeta(rootStyle)
      autoStyle <- parseBody(rawAutoStyle)(style, logger)

      rawBody <- root \!! OdtTags.Body
      rawScripts <- root \!! OdtTags.Scripts
      scriptsNode <- parseBody(rawScripts)(style, logger)
      rawStyleInBody <- root \!! OdtTags.AutomaticStyle
      styleInBody <- parseBody(rawStyleInBody)(style, logger)
    } yield {
      val style1 = StyleFactory().loadFromStyleDoc(root, style)
      val bodyNodes = rawBody.child.flatMap(parseBody(_)(style1, logger))
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


  def loadDocStyleFromMeta(node: scala.xml.Node)(implicit logger: Logger): Option[(Repr.Aux[Underlying], DocumentStyle.Aux[Underlying])] = {
    def length(prop: Option[String]): Option[Int] =
      prop.toRight("0cm").fold(inCm, inCm)

    val doc = (for {
      rawHeader  <- node \!! (OdtTags.MasterStyle / OdtTags.MasterPage / OdtTags.StyleHeader)
      rawFooter  <- node \!! (OdtTags.MasterStyle / OdtTags.MasterPage / OdtTags.StyleFooter)
      pgLayout<- node \!! (OdtTags.AutomaticStyle / OdtTags.StylePageLayout)
    } yield {
      val f = StyleFactory()
      for {
        marginLeft   <- length(pgLayout.attributes.getTag(OdtTags.FoMarginLeft))
        marginRight  <- length(pgLayout.attributes.getTag(OdtTags.FoMarginRight))
        paddingLeft  <- length(pgLayout.attributes.getTag(OdtTags.FoPaddingLeft))
        paddingRight <- length(pgLayout.attributes.getTag(OdtTags.FoPaddingRight))
        pageWidth    <- length(pgLayout.attributes.getTag(OdtTags.FoPageWidth))
        header       <- parseBody(rawHeader)(DocumentStyle.empty, implicitly[Logger])
        footer       <- parseBody(rawFooter)(DocumentStyle.empty, implicitly[Logger])
      } yield {
        val w =
          pageWidth - (marginLeft + marginRight +
          paddingLeft + paddingRight)

        val emptyStyleSheet = DocumentStyle(header, footer, w)

        val docWithStyles = f.loadFromStyleDoc(node, emptyStyleSheet)

        logger.debug(s"Loaded style:\n${docWithStyles}")

        docWithStyles
      }
    }).flatten

    val node1 = node.wrapRec(Tag.nodeTag)

    doc map ((node1, _))
  }

  def parseBody(node: scala.xml.Node)(implicit docStyle: DocumentStyle.Aux[Underlying], logger: Logger): Option[Repr.Aux[Underlying]] =
    node match {
      case Text(text) =>
        Some(node.wrap(tag = Tag.textTag, body = Nil, contents = Some(text)))
      case _ =>
        parseBodyElement(node)
    }

  private def parseBodyElement(node: Underlying)(implicit docStyle: DocumentStyle.Aux[Underlying], logger: Logger): Option[Repr.Aux[Underlying]] = {
    // TODO: handle lists

    implicit def toOpt[T](x: Repr.Aux[T]): Option[Repr.Aux[T]] =
      Some(x)

    lazy val children = node.child.flatMap(parseBody)
    implicit val source: Underlying = node

    lazy val sty: Style = {
      lazy val styleIdOpt = StyleId.forNonStyleNode(node)
      (for {
        styleId <- styleIdOpt
        style   <- docStyle.style(styleId)
      } yield style).getOrElse {
        logger.info(s"no style for ${styleIdOpt}")
        Style.empty
      }
    }

    node.xmlTag match {
      case OdtTags.Tab =>
        val tabsNum = node.attributes.getTag(OdtTags.C).map(_.toInt).getOrElse(1)
        val tabNodes =
          Repr.makeTextElem[Underlying](tabEncoded * tabsNum, synthetic = true)

        Repr.makeElem(Tag.nodeTag, tabNodes +: children)

      case OdtTags.S =>
        val spaces =
          node.attributes.getTag(OdtTags.C).map(_.toInt).getOrElse(1)

        val whitespaceNodes =
          Repr.makeTextElem[Underlying](spaceEncoded * spaces, synthetic = true)
        Repr.makeElem(Tag.nodeTag, whitespaceNodes +: children)

      case OdtTags.Linebreak =>
        Repr.makeTextElem[Underlying](linebreakEncoded, synthetic = true)

      case OdtTags.H =>
        // TODO: why on non-blank we wrap it?
        logger.debug(s"[parser] header tag: $node")
        for {
          styleTpe <- sty.tpe if !(node isBlank)
        } yield node.wrap(tag = styleTpe.tag, body = children)

      case OdtTags.List =>
        // attr:
        // - start
        // - data-continue-list

        // TODO: FIX
        logger.warn(s"Ignoring List")
        None

      case OdtTags.TextListItem =>
        node.wrap(tag = Tags.LI, body = children)

      case OdtTags.Annotation =>
        node.wrap(tag = Tags.ASIDE, body = children)

      case OdtTags.Creator | OdtTags.NoteCitation | OdtTags.BookmarkEnd =>
        // TODO: include the potential child nodes
        None

      case t @ OdtTags.Note =>
        // TODO: bring back safe-checks
        children match {
          case node :: Nil => node
          case _           => None
        }

      case OdtTags.NoteBody =>
        // TODO: postprocessing should get rid of the whitespaces
        Repr.makeElem(Tags.FOOTNOTE, children)

      case OdtTags.P =>
        // infer indentation level from the style
        val indentLvl =
          First(sty.marginLeft) |+| First(sty.textIndent)

        Some(scalaz.Tag.unwrap(indentLvl) map { lvl =>
          val attr1 = Attribute(InternalAttributes.indent, lvl.toString) :: Nil
          Repr.makeElem(tag = Tags.BLOCK, children, attr1)
        } getOrElse (node.wrap(tag = Tags.P, body = children)))

      case OdtTags.Span =>

        // Translate attributes into individual, nested nodes
        val body1 = translateStyleToTags(children, styleToTagsMap, sty)
        val body2 = sty.fontFamily.map(font =>
          if (Utils.isCodeFont(font)) Repr.makeElem(Tags.CODE, body1) :: Nil
          else body1)

        Repr.makeElem(tag = Tags.SPAN, body = body2.getOrElse(body1))

      case OdtTags.A =>
        val tpeAttr = AttributeKey(OdtTags.HrefType)
        // TODO: re-enable the assert
        //assert((tpeAttr inAttributes (attr)).getOrElse("") == "simple")

        val body = children.flatMap(child =>
          whack(child, _ hasTag (Tags.SPAN | Tags.U)))
        Repr.makeElem(tag = Tags.A, body)

      case OdtTags.BookmarkStart =>
        // TOOD: Missing guards
        val attrs = node.attributes.getTag(OdtTags.TextNameAttr).
          map(v => Attribute(InternalAttributes.href, v) :: Nil).getOrElse(Nil)
        node.wrap(tag = Tags.A, body = children, attributes = attrs)

      case OdtTags.Table =>
        logger.warn(s"[limitation] Ignoring Table node")
        // TODO: ignore for the moment
        None

      case OdtTags.TableRow =>
        node.wrap(tag = Tags.TR, body = children)

      case OdtTags.TableCell =>
        node.wrap(tag = Tags.TD, body = children)

      case OdtTags.TableColumn =>
        node.wrap(tag = Tags.COL, body = children)

      case OdtTags.Frame =>
        // TODO: ignore for the moment
        logger.warn(s"Ignoring Frame node")
        //Utils.makeFigure[Underlying](???,
        //  ???, children, FigureInfo.fromNode(node))
        None

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

  // TODO: avoid double pass-through
  // Currently just pass-through
  private def parseStyleNode(node: scala.xml.Node)(implicit sty: DocumentStyle.Aux[Underlying], logger: Logger): Option[Repr.Aux[Underlying]] = {
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
              // Leave the style attribute, as is.
              //src <- orig.withAttribute(Attribute(attributeTag, val2Tag.value.name), acc)
            } yield Seq(Repr.makeElem(tag = val2Tag.tag, body = acc)(orig, wrapper))
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
    StyleToTag[StylePropKey.Underline.type] !:
      StyleToTag[StylePropKey.FontWeight.type] !:
        StyleToTag[StylePropKey.FontStyleProp.type] !:
          StyleToTag[StylePropKey.LineThrough.type] !:
            StyleToTag[StylePropKey.TextPosition.type] !:
              HNil

  private val styleToTagsMap: StyleToTags =
    StyleToTag(StylePropKey.Underline)(ValToTag[attributes.Underline](attributes.Underline.Solid, Tags.U) :: Nil) ::
      StyleToTag(StylePropKey.FontWeight)(ValToTag[attributes.FontWeight](attributes.FontWeight.Bold, Tags.B) :: Nil) ::
        StyleToTag(StylePropKey.FontStyleProp)(ValToTag[attributes.FontStyle](attributes.FontStyle.Italic, Tags.I) :: Nil) ::
          StyleToTag(StylePropKey.LineThrough)(ValToTag[attributes.LineThrough](attributes.LineThrough.Solid, Tags.S) :: Nil) ::
            StyleToTag[StylePropKey.TextPosition.type](StylePropKey.TextPosition)(ValToTag(attributes.TextPosition.Sub, Tags.SUB) :: ValToTag(attributes.TextPosition.Sup, Tags.SUP) :: Nil) ::
              HNil

  private implicit lazy val odtNodeFactory: NodeFactory[scala.xml.Node] =
    new OdtNodeFactory

  private final val sizeP = """(\d+)cm""".r

  private final val spaceEncoded = " &nbsp;"
  private final val linebreakEncoded = "br"
  private final val tabEncoded = " \\t"

  private def inCm(v: String): Option[Int] =
    sizeP.findFirstMatchIn(v) match {
      case Some(sizeP(size)) => Some(size.toInt)
      case res               => None
    }
}
