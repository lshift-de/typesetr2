package net.lshift.typesetr

import io.circe.Json

import java.io.{ FileOutputStream, OutputStream, PrintWriter, File }

import cmd.InputFormat.Markdown
import cmd._
import net.lshift.typesetr.styles.MetaFromDocument
import pandoc.writers.latex.LatexWriter
import parsers.OdtParser
import util.Logger
import writers.odt.OdtWriter
import org.apache.commons.io.FilenameUtils

import scala.language.postfixOps

/**
 * The main entry point to running typesetr pipeline.
 * Running the converter without any options set will
 * actually
 */
object Converter {

  def main(args: Array[String]): Unit = {
    CommandParser().parse(args) match {
      case Some(config) =>
        implicit val logger = Logger(config.logging)

        val result = for {
          inputFile <- retrieveInputFile(config).right
          outputFile <- retrieveOutputFile(config).right
        } yield {
          logger.info(s"Processing for $inputFile")

          // 1. Infer the input-specific parser
          val (parser, writer) = config.inFormat match {
            case InputFormat.Odt =>
              (new OdtParser(), new OdtWriter(inputFile))
            case InputFormat.Docx =>
              (???, ???)
            case InputFormat.Markdown =>
              (???, ???)
            case InputFormat.Html =>
              (???, ???)
          }

          // 2. Parse the document and produce an internal
          // representation of the document
          val parsed = parser.parse(inputFile, makeTransclusions = false)
          // 3. Optimize the document
          val optimizer = postprocessors.DefaultPostProcessor.fromConfig(parser.nodeConfig)
          val optimizedDoc =
            if (config.Yoptimize)
              optimizer.optimize(parsed.root)
            else
              parsed.root

          val (cleansedDoc, docMeta) = optimizer.inferMeta(optimizedDoc)(parsed.style)

          // 4. Store the optimized document
          val optimizedInput =
            writer.writeToFile(cleansedDoc)(logger, config)

          // 5. Translate the optimized document using pandoc
          // 6. Apply Typesetr's templates
          val result = for {
            pandocInputF <- optimizedInput.toRight("No input found").right
            pandocOutput <- pandocTranslation(config.inFormat,
              outputFile.format,
              pandocInputF).right
            generator <- pandocPostprocessor(pandocOutput, outputFile.outputFile,
              config, docMeta).right
          } yield {
            generator.write(config)
          }

        }

        result match {
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
   * @param outFormat - desired output format
   * @param inFile - Typesetr's optimized input document
   * @return the temporary file representing the resulting document or an error message.
   */
  def pandocTranslation(inF: InputFormat, outFormat: OutputFormat, inFile: File): Either[String, File] = {
    import sys.process._

    val (opts, outF) = outFormat match {
      case OutputFormat.Pdf =>
        (List[String](), OutputFormat.Tex)
      case _ =>
        (Nil, outFormat)
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
   * @param inFile input file to the generator
   * @param outFile desired output file of the document generation
   * @param config converter configuration
   * @param docMeta meta information inferred from the document
   * @return a generic document generator.
   */
  def pandocPostprocessor(inFile: File, outFile: File, config: Config, docMeta: MetaFromDocument): Either[String, pandoc.Writer] = {
    val template = config.inFormat match {
      case InputFormat.Odt =>
        styles.StyleTemplate.odt(config.styleBase, config.style)
      // TODO: other templating formats
      case _ =>
        ???
    }
    config.outFormat match {
      case OutputFormat.Pdf =>
        Right(new LatexWriter(inFile, outFile, template, docMeta, true))
      case OutputFormat.Tex =>
        Right(new LatexWriter(inFile, outFile, template, docMeta, false))
      case format =>
        Left("No postprocessor for the $format format")
    }
  }

  def rewriteInput(config: Config): Option[String] = {
    // TODO
    Some("")
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
        //val rewriteF = File.createTempFile("rewritten", extIn)
        //logger.info(s"Rewrite input to ${rewriteF}")

        Right(ProcessingFileInfo(format, fOut))
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
  //def rewriteInputTo: File
  def outputFile: File
}

object ProcessingFileInfo {

  def apply(format: OutputFormat, outputFile: File): ProcessingFileInfo =
    FProcessingFile(format, outputFile)

  private case class FProcessingFile(format: OutputFormat,
                                     outputFile: File) extends ProcessingFileInfo

}

