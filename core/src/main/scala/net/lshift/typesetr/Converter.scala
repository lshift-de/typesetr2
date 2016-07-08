package net.lshift.typesetr

import io.circe.Json

import java.io.{ FileOutputStream, OutputStream, PrintWriter, File }

import net.lshift.typesetr.cmd.{ CompressKind, OutputFormat, CommandParser, Config }
import net.lshift.typesetr.util.Logger
import org.apache.commons.io.FilenameUtils


/* *
 * The main entry point to triggering the converter
 *
 */
object Converter {

  def main(args: Array[String]): Unit = {
    CommandParser().parse(args) match {
      case Some(config) =>
        val logger = Logger(config.error)
        
        // Start the external process
        for {
          _ <- rewriteInput(config).toLeft("").right
          styleTempl <- retrieveStyle(config).right
          inputFile <- retrieveInputFile(config).right
          output <- retrieveOutputFile(config).right
        } yield {
          logger.info(s"Processing for $inputFile in ${output._2}")

        }

      case None =>
    }

    println("TODO")
  }

  def rewriteInput(config: Config): Option[String] =
    // TODO
    Some("")

  def retrieveStyle(config: Config): Either[String, StyleTemplate] = {
    val template = StyleTemplate(config.style, config.styleBase)
    if (template.exists)
      Right(template)
    else {
      // TODO: implement the fallback mechanism that checks for
      //       available styles and tries to match them
      //       against the provided one.
      Left(s"Style ${config.style} is invalid")
    }
  }

  def retrieveInputFile(config: Config): Either[String, File] = {
    Right(config.inFile.getOrElse({
      
      // Read from a standard input and save to
      // a temporary file
      val conts = scala.io.Source.stdin.getLines().mkString(System.lineSeparator)
  
      // Identify the extension of the file
      import DocumentKind._
      val kind = conts match {
        case OdtDocument(_)  => OdtDocument
        case DocxDocument(_) => DocxDocument
        case HtmlDocument(_) => HtmlDocument
        case _               => MdDocument
      }
      val f = java.io.File.createTempFile("typeseter", kind.suffix)
      val pw = new PrintWriter(f)
      pw.write(conts)
      pw.close
      f
    }))
  }

  def retrieveOutputFile(config: Config): Either[String, (OutputStream, OutputFormat)] = {
    config.outFile match {
      case Some(f0) =>
        val fPath0 = f0.getAbsolutePath
        val ext = FilenameUtils.getExtension(fPath0)
        val (fPath, format) = ext match {
          case CompressKind.Zip.repr =>
            assert(config.compress.nonEmpty && (config.compress.get == CompressKind.Zip))
            val fPath1 = FilenameUtils.getFullPath(fPath0)
            val ext1 = FilenameUtils.getExtension(fPath1)
            (fPath1, OutputFormat.unapply(ext1).get)
          case _ =>
            (fPath0, config.format)
        }
        // TODO: some packaging hacks missing
        val f = new File(fPath)
        f.createNewFile()
        Right((new FileOutputStream(f), format))
      case None =>
        Right((System.out, config.format))
    }
  }

}

sealed abstract class DocumentKind {
  def suffix: String
}

object DocumentKind {

  def unapply(x: DocumentKind): Option[String] = Some(x.suffix)

  case object OdtDocument extends DocumentKind {
    def suffix = "odt"
    def unapply(src: String): Option[String] = ???
  }

  case object DocxDocument extends DocumentKind {
    def suffix = "docx"
    def unapply(src: String): Option[String] = ???
  }

  object HtmlDocument extends DocumentKind {
    def suffix = "html"
    def unapply(src: String): Option[String] = ???
  }

  object MdDocument extends DocumentKind {
    def suffix = "md"
  }

}