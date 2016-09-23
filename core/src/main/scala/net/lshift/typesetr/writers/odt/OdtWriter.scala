package net.lshift.typesetr
package writers
package odt

import java.io.{ File, FileOutputStream }
import java.nio.channels.Channels

import cmd.Config
import net.lshift.typesetr.parsers.odt.OdtTags
import parsers.Repr.Aux
import util.Logger
import xml.{ InternalAttributes, Tag }

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

    val outS = new FileOutputStream(f)
    val writer = Channels.newWriter(outS.getChannel(), TextEncoding)

    val rewriteTo = File.createTempFile("rewritten", "-typesetr.odt")
    val stream = new FileOutputStream(rewriteTo)

    logger.info(s"Rewritten .odt file @ $rewriteTo")

    try {
      writer.write("<?xml version='1.0' encoding='" + TextEncoding + s"'?>$NewLine")
      writeNode(node, indent = 0)(config, writer, implicitly[Logger])

      // 2. Pack into an .odt binary
      (for {
        odtFile <- inputFile.unpack()
      } yield {
        val packed = f.pack(stream, odtFile)
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
      writer.close()
      stream.close()
    }

  }

  private def writeNode(node: Aux[N], indent: Int)(
    implicit config: Config,
    writer: java.io.Writer,
    logger: Logger): Boolean = {

    val space = BlankSpace * (Indent * indent)
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
            writer.write(s"$space$encoded$NewLine")
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
            writer.write(s"$space<${n.prefix}:${n.label}$attrbs $scope/>$NewLine")
          case _ =>
            writer.write(s"$space<${n.prefix}:${n.label}$attrbs $scope>$NewLine")
            node.body.forall(writeNode(_, indent + 1))
            writer.write(s"$space</${n.prefix}:${n.label}>$NewLine")
        }
        true
    }
  }

  private def translateAttributes(node: Aux[N], indent: Int)(
    implicit config: Config, writer: java.io.Writer, logger: Logger): N = {

    def translateAttribute(node: N, attr: List[xml.Attribute]): N = attr match {
      case Nil => node
      case xml.Attribute(InternalAttributes.indent, v) :: rest =>
        // TODO: create an indentation if style does not have it.
        translateAttribute(node, rest)
      case xml.Attribute(InternalAttributes.style, styleId) :: rest =>
        translateAttribute(node.copy(meta =
          node.attributes.copyWith(OdtTags.StyleNameAttr, styleId)), rest)
      case _ :: rest =>
        translateAttribute(node, rest)
    }

    translateAttribute(node.source, node.attr)
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

  private final val BlankSpace = " "

  private final val NoSpace = ""

  private final val NewLine = "\n"

  private final val TextEncoding = "UTF-8"

}