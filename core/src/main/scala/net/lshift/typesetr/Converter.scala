package net.lshift.typesetr

import io.circe.Json

import java.io.{ FileOutputStream, OutputStream, PrintWriter, File }

import net.lshift.typesetr.cmd.InputFormat.Markdown
import net.lshift.typesetr.cmd._
import net.lshift.typesetr.parsers.odt.OdtWriter
import net.lshift.typesetr.parsers.{ NodeRepr, OdtParser }
import net.lshift.typesetr.postprocessors.Optimizer
import net.lshift.typesetr.util.Logger
import org.apache.commons.io.FilenameUtils
import sun.awt.SunToolkit.OperationTimedOut

/* *
 * The main entry point to triggering the converter
 *
 */
object Converter {

  def main(args: Array[String]): Unit = {
    CommandParser().parse(args) match {
      case Some(config) =>
        implicit val logger = Logger(config.error)

        // Start the external process
        val ret = for {
          _ <- rewriteInput(config).toLeft("").left
          styleTempl <- retrieveStyle(config).right
          inputFile <- retrieveInputFile(config).right
          outputFile <- retrieveOutputFile(config).right
        } yield {
          logger.info(s"Processing for $inputFile")

          val (parser, writer) = config.inFormat match {
            case InputFormat.Odt =>
              (new OdtParser(), new OdtWriter())
            case InputFormat.Docx =>
              (???, ???)
            case InputFormat.Markdown =>
              (???, ???)
          }

          val parsed = parser.parseToRawBody(inputFile, false, false)
          val optimized =
            if (config.Yoptimize)
              Optimizer().process(parsed)(parser.wrapper, logger)
            else
              parsed

          // temporarily write to file
          val res = writer.writeToFile(optimized, outputFile.tmpFile)(
            logger, config)

          config.inFormat match {
            case InputFormat.Odt =>
              for {
                odtFile <- inputFile.unpack()
              } yield {
                val packed = outputFile.tmpFile.pack(
                  outputFile.stream, odtFile)
                if (!packed)
                  println("FAILED TO PACK in odt")
              }
            case _ =>
              ???
          }

        }

        ret match {
          case Left(errMsg) =>
            logger.fail(errMsg)
          case _ =>
        }

      case None =>
    }
  }

  def rewriteInput(config: Config): Option[String] = {
    // TODO
    Some("")
  }

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

      val kind = conts match {
        case InputFormat(format) =>
          format
        case _ =>
          // TODO: produce some reasonable error message
          ???
      }

      val f = java.io.File.createTempFile("typeseter", kind.suffix)
      val pw = new PrintWriter(f)
      pw.write(conts)
      pw.close
      f
    }))
  }

  def retrieveOutputFile(config: Config)(implicit logger: Logger): Either[String, OutputInfo] = {
    logger.info(s"Determine output file from ${config.outFile}")
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
            (fPath0, config.outFormat)
        }
        // TODO: some packaging hacks missing
        val f = new File(fPath)
        f.createNewFile()

        val tmpFile =
          if (config.Yns) {
            File.createTempFile("typesetr", ".xml")
          } else {
            val dir = new File("/tmp/styles")
            dir.mkdirs()
            val f = new File("/tmp/styles/typesetr.xml")
            f.createNewFile()
            f
          }
        logger.info(s"Temporary output file: ${tmpFile}")
        Right(FOutput(format, new FileOutputStream(f), tmpFile))
      case None =>
        // TODO:
        Left("Implementation limitation - cannot write to stdout")
    }
  }

}

// TODO: needs better documentation
abstract class OutputInfo {
  def format: OutputFormat
  def stream: FileOutputStream
  def tmpFile: File
}

case class FOutput(format: OutputFormat,
                   stream: FileOutputStream,
                   tmpFile: File)
  extends OutputInfo
