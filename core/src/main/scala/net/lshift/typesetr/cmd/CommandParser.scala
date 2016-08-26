package net.lshift.typesetr.cmd

import java.io.{ FileReader, InputStream, File }

import io.circe.Json
import net.lshift.typesetr._
import scopt.Read

import scala.reflect.macros.ParseException

trait CommandParser {
  def parse(args: Array[String]): Option[Config]
}

object CommandParser extends OptReaders {

  def apply(): CommandParser = new CommandParser {
    def parse(args: Array[String]): Option[Config] = {
      parser.parse(args, Config())
    }
  }

  private lazy val parser = new scopt.OptionParser[Config]("typeseter-converter") {
    head("converter", "0.2")

    opt[InputFormat]('i', "inFormat").required().action((x, c) =>
      c.copy(inFormat = x))
      .valueName("[odt, docx, md]]")
      .text("The input format you'd like to convert from")

    opt[OutputFormat]('o', "outFormat").required().action((x, c) =>
      c.copy(outFormat = x))
      .valueName("[tex, pdf, png, html, epub, meta, internal, pickle]")
      .text("The output format you'd like to convert to")

    opt[Option[File]]("infile").optional().action((x, c) =>
      c.copy(inFile = x))
      .text("The file to convert (default: stdin)")

    opt[Option[File]]("outfile").optional().action((x, c) =>
      c.copy(outFile = x))
      .text("The name of the output file (default: stdout)")

    opt[Style]('s', "style").required().action((x, c) =>
      c.copy(style = x))
      .text("The style (&type) of document to create")

    opt[File]("style-base").required().action((x, c) =>
      c.copy(styleBase = x))
      .text("Where to find the styles")
      .validate(v =>
        if (v.isDirectory) success
        else failure(s"Invalid path to the styles' directory: $v"))

    opt[Int]("page").optional().action((x, c) =>
      c.copy(page = x))
      .text("One-based index of the page to render")

    opt[Int]("pixels").optional().action((x, c) =>
      c.copy(pixels = x))
      .text("PNG image size")

    opt[Boolean]("Yopt").optional().action((x, c) =>
      c.copy(Yoptimize = x))
      .text("Optimize the initial document")

    opt[Boolean]("Yns").optional().action((x, c) =>
      c.copy(Yns = x))
      .text("Include namespace info")

    opt[Boolean]("no-clean").optional().action((x, c) =>
      c.copy(noClean = x))
      .text("Do not remove temporary files after success")

    opt[io.circe.Json]("gdoc-meta").optional().action((x, c) =>
      c.copy(gdocMeta = x))
      .text("The metadata from google in JSON form")

    opt[List[GDocIdentifier]]('i', "include").optional().action((x, c) =>
      c.copy(include = x))
      .text("The gdoc id/url of the include files")

    opt[Option[YamlString]]("new-meta").optional().action((x, c) =>
      c.copy(newMeta = x))
      .text("New (YAML) meta data for the document to update")

    opt[Option[File]]("rewritten-input").optional().action((x, c) =>
      c.copy(rewrittenInput = x))
      .text("In combination w/ --new-meta: write out an updated input file")

    opt[Boolean]("lofi").optional().action((x, c) =>
      c.copy(noClean = x))
      .text("Rescale all images to very low resolution; for testing only")

    opt[Option[CompressKind]]("compress").optional().action((x, c) =>
      c.copy(compress = x))
      .text("Compress the output in an archive")

    opt[List[File]]('b', "bibliography").optional().action((x, c) =>
      c.copy(bibliography = x))
      .text("The bibliography to use, if any")
      .validate(x =>
        if (verifyBibtex(x)) success
        else failure(s"Unable to process at least one of the bibtext files in '$x'"))

    opt[Boolean]("comments").optional().action((x, c) =>
      c.copy(comments = x))
      .text("Whether to preserve comments")

    opt[LogLevel]("log").optional().action((x, c) =>
      c.copy(logging = x))
      .valueName("[none, log, raise, debug]")
      .text("For debugging: what to do on document errors")

    checkConfig(c =>
      if (c.rewrittenInput.isDefined && c.newMeta.isEmpty)
        failure("Updated input has to be associated with new metadata information")
      else
        success)

    checkConfig(c => {
      val f = c.styleBase \ c.style.toString
      if (f.isDirectory) success
      else failure(s"Unable to find the source of ${c.style} style in ${c.styleBase.getAbsolutePath}")
    })

  }

  private def verifyBibtex(files: List[File]): Boolean = {
    def verify(f: File): Boolean = {
      val reader = new FileReader(f)
      val bibtexParser = new org.jbibtex.BibTeXParser()
      try {
        bibtexParser.parse(reader)
        true
      } catch {
        case _: ParseException => false
      } finally {
        reader.close()
      }
    }
    files forall verify
  }
}

trait OptReaders {

  implicit def toJsonRead: Read[Json] = Read.reads { (v: String) => Json.fromString(v) }

  implicit def toListRead[T](implicit gen: Read[T]): Read[List[T]] = Read.reads { (v: String) =>
    v.split(",").toList.map(v => gen.reads(v.trim()))
  }

  implicit def toOptRead[T](implicit gen: Read[T]): Read[Option[T]] = Read.reads { (v: String) =>
    Some(gen.reads(v))
  }

}
