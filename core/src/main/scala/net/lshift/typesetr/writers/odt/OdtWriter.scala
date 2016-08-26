package net.lshift.typesetr
package writers
package odt

import java.io.{ File, FileOutputStream }
import java.nio.channels.Channels

import cmd.Config
import parsers.Repr.Aux
import util.Logger
import xml.{ InternalAttributes, Tag }

import scala.annotation.tailrec
import scala.xml._

class OdtWriter extends Writer {
  type N = scala.xml.Node

  import OdtWriter._

  def writeToFile(node: Aux[N])(implicit logger: util.Logger, config: Config): Option[java.io.File] = {

    val pp = new PrettyPrinter(80, 2)

    val f =
      if (config.Yns) {
        File.createTempFile("content", "xml")
      } else {
        val dir = new File("/tmp/styles")
        dir.mkdirs()
        val f = new File("/tmp/styles/typesetr.xml")
        f.createNewFile()
        f
      }

    val outS = new FileOutputStream(f)
    val writer = Channels.newWriter(outS.getChannel(), TextEncoding)

    try {
      writer.write("<?xml version='1.0' encoding='" + TextEncoding + s"'?>$NewLine")
      writeNode(node, indent = 0)(pp, config, writer, implicitly[Logger])
      Some(f)
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        None
    } finally {
      writer.close()
    }

  }

  private def writeNode(node: Aux[N], indent: Int)(
    implicit pp: PrettyPrinter,
    config: Config,
    writer: java.io.Writer,
    logger: Logger): Boolean = {

    val space = BlankSpace * (Indent * indent)
    node.tag match {
      // Ignore synthetic tags, they are only create
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

        translateAttributes(node, indent)
        // TODO: Analyze the attributes of our node
        // this may lead to some style changes
        val n = node.source
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
    implicit pp: PrettyPrinter, config: Config,
    writer: java.io.Writer, logger: Logger): Boolean = {

    def translateAttribute(attr: List[xml.Attribute]): Unit = attr match {
      case Nil =>
      case xml.Attribute(InternalAttributes.indent, v) :: rest =>
        // TODO: create an indentation if style does not have it.
        translateAttribute(rest)
      case _ :: rest => // other internal attributes
    }

    translateAttribute(node.attr)
    true
  }

  private def filterNamespaceBinding(scope: NamespaceBinding,
                                     filtered: List[String] = Nil): NamespaceBinding =
    if (scope.parent == null) null
    else if (scope.prefix == "loext:contextual-spacing")
      filterNamespaceBinding(scope.parent, filtered)
    else
      NamespaceBinding(scope.prefix, scope.uri,
        filterNamespaceBinding(scope.parent, filtered))

  private def translateMeta(m: scala.xml.MetaData): String = {
    @tailrec
    def translateMeta0(m: scala.xml.MetaData, pre: List[String]): List[String] =
      if (m == null) pre
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