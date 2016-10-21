package net.lshift.typesetr
package writers
package odt

import java.io.{ File, FileOutputStream }
import java.nio.channels.Channels

import cmd.Config
import net.lshift.typesetr.pandoc.Markers
import net.lshift.typesetr.parsers.odt.OdtTags
import parsers.Repr.Aux
import util.Logger
import net.lshift.typesetr.xml.{ InternalTags, InternalAttributes, Tag }

import scala.annotation.tailrec
import scala.xml._

class OdtWriter(inputFile: File) extends Writer {
  type N = scala.xml.Node

  import OdtWriter._

  def writeToFile(node: Aux[N])(implicit logger: util.Logger, config: Config): Option[java.io.File] = {

    val f =
      if (config.Yns) {
        File.createTempFile("content", ".xml")
      } else {
        val dir = new File("/tmp/styles")
        dir.mkdirs()
        val f = new File("/tmp/styles/typesetr.xml")
        f.createNewFile()
        f
      }

    logger.info(s"New content of the .odt file is located @ $f")

    val writer = Channels.newWriter(new FileOutputStream(f).getChannel(), TextEncoding)

    val rewriteTo = File.createTempFile("rewritten", "-typesetr.odt")
    val stream = new FileOutputStream(rewriteTo)

    logger.info(s"Rewritten .odt file @ $rewriteTo")

    try {
      writer.write("<?xml version='1.0' encoding='" + TextEncoding + s"'?>$NewLine")
      writeNode(node, indent = 0)(config, writer, implicitly[Logger])
      writer.close()

      // 2. Pack into an .odt binary
      (for {
        (odtFileMap, odtDir) <- inputFile.unpack()
      } yield {
        logger.info(s"Temporary .odt directory: $odtDir")
        val packed = f.pack(stream, odtFileMap)
        if (!config.Ytmp)
          odtDir.deleteDirectory()

        if (!packed) {
          logger.info("Failed to create an ODT file")
          None
        } else Some(rewriteTo)

      }).flatten
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        None
    } finally {
      if (!config.Ytmp)
        f.delete()
      stream.close()
    }

  }

  private def writeNode(node: Aux[N], indent: Int)(
    implicit config: Config,
    writer: java.io.Writer,
    logger: Logger): Boolean = {

    val space = BlankSpace * (Indent * indent)
    val docSpace = if (config.YprettyPrint) BlankSpace * (Indent * indent) else ""
    val docNewLine = if (config.YprettyPrint) NewLine else ""
    node.tag match {

      case Tag.syntheticTextTag =>
        true

      case Tag.textTag =>

        node.contents match {
          case Some(text) =>
            val encoded = Encoding.foldLeft(text) {
              case (t, (from, to)) =>
                t.replaceAll(from, to)
            }

            // Make sure that \ are properly quoted.
            val encoded1 = inlineMath.replaceAllIn(encoded,
              m => scala.util.matching.Regex.quoteReplacement(
                Markers.mathBlock(m.group(inlineMathGroup))))
            writer.write(s"$space$encoded1$NewLine")
            true
          case _ =>
            writer.write(s"<missing text>$NewLine")
            // todo: handle synthetic text nodes
            false
        }

      case _ =>

        val n = translateAttributes(node, indent)
        val scope =
          if (config.Yns) filterNamespaceBinding(n.scope) else NoSpace
        val attrbs = translateMeta(n.attributes)

        node.body match {
          case Nil if node.contents.isEmpty =>
            writer.write(s"$docSpace<${n.prefix}:${n.label}$attrbs $scope/>$docNewLine")
          case _ =>
            writer.write(s"$docSpace<${n.prefix}:${n.label}$attrbs $scope>$docNewLine")
            node.tag match {
              case InternalTags.CODE =>
                val x = node.getAttribute(InternalAttributes.indent).flatMap(_.value).map(_.toInt).getOrElse(0)
                if (x > 1) {
                  writer.write(s"$docSpace${(pandoc.Writer.TypesetrPreSpace * x).toString}")
                  node.body.forall(writeNode(_, 0))
                } else
                  node.body.forall(writeNode(_, indent + 1))
              case _ =>
                node.body.forall(writeNode(_, indent + 1))
            }
            writer.write(s"$docSpace</${n.prefix}:${n.label}>$docNewLine")
        }
        true
    }
  }

  private def translateAttributes(nodeRepr: Aux[N], indent: Int)(
    implicit config: Config, writer: java.io.Writer, logger: Logger): N = {

    def translateAttribute(node: N, attr: List[xml.Attribute]): N = attr match {
      case Nil => node
      case xml.Attribute(InternalAttributes.indent, v) :: rest =>
        // TODO: create an indentation if style does not have it.
        translateAttribute(node, rest)
      case xml.Attribute(InternalAttributes.style, styleId) :: rest =>
        translateAttribute(node.copy(meta =
          node.attributes.copyWith(OdtTags.StyleNameAttr, styleId)), rest)
      case xml.Attribute(InternalAttributes.outlineLvl, lvl) :: rest =>
        translateAttribute(node.copy(meta =
          node.attributes.copyWith(OdtTags.TextOutlineLevel, lvl)), rest)
      case xml.Attribute(InternalAttributes.imgWidth, width) :: rest =>
        translateAttribute(node.copy(meta =
          node.attributes.copyWith(OdtTags.SvgWidth, s"$width%"). // Pandoc understands '%'
            remove(OdtTags.SvgHeight)), rest)
      case xml.Attribute(InternalAttributes.href, href) :: rest =>
        nodeRepr.tag match {
          // Note: When using bookmarks for labels Pandoc introduces
          //       its own pretty-printed anchors. Sadly the renaming is
          //       is not consistent (looks like a bug in the odt parser)
          //       so the bookmark references just refer to some new anchors
          //       rather than those introduced in the labels.
          //       Using references fixes things in a sense that for some reason
          //       Pandoc stops introducing renaming for the labels (but keeps
          //       doing it for the references (see 'read_reference_start' method
          //       in Pandoc).
          case InternalTags.LABEL =>
            new Elem(
              prefix = OdtTags.ReferenceStart.namespace.short.value,
              label = OdtTags.ReferenceStart.tag,
              attributes1 = node.attributes.copyWith(OdtTags.TextNameAttr, href),
              minimizeEmpty = node.asInstanceOf[Elem].minimizeEmpty,
              scope = node.scope,
              child = (node.child: _*))
          case InternalTags.A if href.startsWith("#") =>
            // Normally that should be the correct way.
            // But Pandoc's bug is preventing us from "doing the right thing".
            // (see above comment)
            /*new Elem(
              prefix = OdtTags.ReferenceRef.namespace.short.value,
              label = OdtTags.ReferenceRef.tag,
              attributes1 = node.attributes.copyWith(OdtTags.BookmarkOrReferenceName, "anchor-" + href),
              minimizeEmpty = node.asInstanceOf[Elem].minimizeEmpty,
              scope = node.scope,
              child = (node.child: _*))*/
            translateAttribute(node, rest)
          case _ =>
            translateAttribute(node, rest)
        }
      case _ :: rest =>
        translateAttribute(node, rest)
    }

    translateAttribute(nodeRepr.source, nodeRepr.attr)
  }

  private def filterNamespaceBinding(scope: NamespaceBinding,
                                     filtered: List[String] = Nil): NamespaceBinding =
    if (scope.parent == null) TopScope
    else if (scope.prefix == "loext:contextual-spacing")
      filterNamespaceBinding(scope.parent, filtered)
    else
      NamespaceBinding(scope.prefix, scope.uri,
        filterNamespaceBinding(scope.parent, filtered))

  private def translateMeta(m: scala.xml.MetaData): String = {
    @tailrec
    def translateMeta0(m: scala.xml.MetaData, pre: List[String]): List[String] =
      if (m == null) pre
      else if (m == scala.xml.Null) pre
      else m match {
        case PrefixedAttribute("loext", "contextual-spacing", v, next) =>
          translateMeta0(next, pre)
        case PrefixedAttribute(p, key, v, next) =>
          translateMeta0(next, s"""$p:$key="$v"""" :: pre)
        case UnprefixedAttribute(key, v, next) =>
          translateMeta0(next, s"""$key="$v"""" :: pre)
        case _ => pre
      }

    translateMeta0(m, Nil) match {
      case Nil   => NoSpace
      case attrs => BlankSpace + attrs.mkString(BlankSpace)
    }
  }

}

object OdtWriter {

  private final val Indent = 4

  private final val Encoding = Seq(
    "&" -> "&amp;",
    "<" -> "&lt;",
    ">" -> "&gt;",
    """"""" -> "&quot;")

  private final val inlineMath = new scala.util.matching.Regex("\\\\\\((.*)\\\\\\)", inlineMathGroup)

  private final val inlineMathGroup = "mathFormula"

  private final val BlankSpace = " "

  private final val NoSpace = ""

  private final val NewLine = "\n"

  private final val TextEncoding = "UTF-8"

}