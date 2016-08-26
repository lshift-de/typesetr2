package net.lshift.typesetr

import io.circe.Json

import java.io.{ FileOutputStream, OutputStream, PrintWriter, File }

import cmd.InputFormat.Markdown
import cmd._
import pandoc.writers.latex.LatexWriter
import parsers.{ NodeFactory, OdtParser }
import postprocessors.Optimizer
import util.Logger
import writers.odt.OdtWriter
import org.apache.commons.io.FilenameUtils

import scala.language.postfixOps

/* *
 * The main entry point to triggering the converter
 *
 */
object Converter {

  def main(args: Array[String]): Unit = {
    CommandParser().parse(args) match {
      case Some(config) =>
        implicit val logger = Logger(config.logging)

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
            case InputFormat.Html =>
              (???, ???)
          }

          val parsed = parser.parseToRawBody(inputFile, false, false)
          val optimized =
            if (config.Yoptimize)
              Optimizer().process(parsed)(parser.wrapper, logger)
            else
              parsed

          // temporarily write to file
          val resContent = writer.writeToFile(optimized)(
            logger, config)

          config.inFormat match {
            case InputFormat.Odt =>
              for {
                odtFile <- inputFile.unpack()
                tmpFile <- resContent
              } yield {
                val stream = new FileOutputStream(outputFile.rewriteInputTo)
                try {
                  val packed = tmpFile.pack(stream, odtFile)
                  if (!packed) logger.info("Failed to create an ODT file")
                } finally {
                  stream.close()
                }
              }
            case _ =>
              ???
          }

          val result = for {
            pandocOutput <- pandocTranslation(config.inFormat,
              outputFile.format,
              outputFile.rewriteInputTo).right
            generator <- pandocPostprocessor(pandocOutput, outputFile.outputFile, config).right
          } yield {
            generator.write(config)
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

  /**
   * Translates a (typesetr-optimized) input file into the target format.
   * Typesetr's templates are not applied yet.
   *
   * @param inF - format of the input document
   * @param outF0 - desired output format
   * @param inFile - Typesetr's optimized input document
   * @return the temporary file representing the resulting document or an error message.
   */
  def pandocTranslation(inF: InputFormat, outF0: OutputFormat, inFile: File): Either[String, File] = {
    import sys.process._

    val (opts, outF) = outF0 match {
      case OutputFormat.Pdf =>
        (List[String](), OutputFormat.Tex)
      case _ =>
        (Nil, outF0)
    }

    val outputFile = File.createTempFile("pandoc-", s"-typesetr.${outF.suffix}")
    val cmd = s"pandoc -f ${inF.suffix} -t ${outF.name} -o ${outputFile.getAbsolutePath} " +
      s"${opts.mkString(" ")} ${inFile.getAbsoluteFile}"

    if ((cmd !) == 0) Right(outputFile)
    else Left("Pandoc failed.")
  }

  /**
   * Infer postprocessor for the pandoc's result from the input/output format.
   *
   * @param inFile - input file to the generator
   * @param outFile - desired output file of the document generation
   * @param config - converter configuration
   * @return a generic document generator.
   */
  def pandocPostprocessor(inFile: File, outFile: File, config: Config): Either[String, pandoc.Writer] =
    config.outFormat match {
      case OutputFormat.Pdf =>
        val template = styles.StyleTemplate.odt(config.styleBase, config.style)
        Right(new LatexWriter(inFile, outFile, template, true))
      case OutputFormat.Tex =>
        val template = styles.StyleTemplate.odt(config.styleBase, config.style)
        Right(new LatexWriter(inFile, outFile, template, false))
      case format =>
        Left("No postprocessor for the $format format")
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

  def retrieveOutputFile(config: Config)(implicit logger: Logger): Either[String, ProcessingFileInfo] = {
    logger.info(s"Determine output file from ${config.outFile}")
    // 1. Typesetr writes the optimized version of the document
    // 2. Pandoc takes 1) and rewrites into the target format
    // 3. Typesetr (optionally) performs additional postprocessing
    // 4. Generate the final file
    (config.inFile, config.outFile) match {
      case (Some(fIn), Some(fOut)) =>
        val extIn = FilenameUtils.getExtension(fIn.getAbsolutePath)
        val fPath0 = fOut.getAbsolutePath
        val extOut = FilenameUtils.getExtension(fPath0)
        val (fPath, format) = extOut match {
          case CompressKind.Zip.repr =>
            assert(config.compress.nonEmpty && (config.compress.get == CompressKind.Zip))
            val fPath1 = FilenameUtils.getFullPath(fPath0)
            val ext1 = FilenameUtils.getExtension(fPath1)
            (fPath1, OutputFormat.unapply(ext1).get)
          case _ =>
            (fPath0, config.outFormat)
        }

        // TODO: some packaging hacks missing
        val rewriteF = File.createTempFile("rewritten", extIn)
        logger.info(s"Rewrite input to ${rewriteF}")

        Right(ProcessingFileInfo(format, rewriteF, fOut))
      case _ =>
        // TODO:
        Left("Implementation limitation - cannot write to stdout/from stdin yet.")
    }
  }

}

/**
 * Configuration for output.
 *
 * - format - final output format, based on the input/output file
 * - rewriteInputTo - the output of the Typesetr's pre-processor
 * - outputFile - desired output file
 */
abstract class ProcessingFileInfo {
  def format: OutputFormat
  def rewriteInputTo: File
  def outputFile: File
}

object ProcessingFileInfo {

  def apply(format: OutputFormat, rewriteInputTo: File, outputFile: File): ProcessingFileInfo =
    FProcessingFile(format, rewriteInputTo, outputFile)

  private case class FProcessingFile(format: OutputFormat,
                                     rewriteInputTo: File,
                                     outputFile: File) extends ProcessingFileInfo

}

