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
 * See command parser for the list of options.
 */
object Converter {

  def main(args: Array[String]): Unit = {
    CommandParser().parse(args) match {
      case Some(config) =>
        implicit val logger = Logger(config.logging)
        implicit val c = config

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

          lazy val optimizer = postprocessors.DefaultPostProcessor.fromConfig(parser.nodeConfig)
          // 2. Parse the document and produce an internal
          // representation of the document
          for {
            parsedDoc <- (parser.parse(inputFile, makeTransclusions = false)).right
            // 3. Optimize the document
            optimizedDoc <- Right(if (config.Yoptimize)
              optimizer.optimize(parsedDoc.root)(implicitly[Logger], parsedDoc.style)
            else
              parsedDoc.root).right
            //inferredMeta <- Right(optimizer.inferMeta(optimizedDoc)(parsedDoc.style)).right
            inferredMeta <- optimizer.inferMeta(optimizedDoc)(parsedDoc.style).right
            // 4. Store the optimized document
            pandocInputF <- writer.writeToFile(inferredMeta._1)(logger, config).
              toRight("Failed to save the optimized document to a file").right
            // 5. Translate the optimized document using pandoc
            pandocOutput <- pandocTranslation(config.inFormat,
              outputFile.format,
              pandocInputF).right
            // 6. Apply Typesetr's templates, configuration
            generator <- pandocPostprocessor(pandocOutput, outputFile.outputFile,
              config, inferredMeta._2, pandocInputF).right
          } yield {
            generator.write(config)
            if (!config.Ytmp) {
              pandocInputF.delete()
              pandocOutput.delete()
            }
          }

        }

        result match {
          case Left(errMsg) =>
            logger.fail(errMsg)
          case _ =>
        }

      case _ =>
      // An error occurred, the problem was already reported
      // by the command parser.
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
  def pandocTranslation(inF: InputFormat, outFormat: OutputFormat, inFile: File)(implicit config: Config): Either[String, File] = {
    import sys.process._

    val tocOpt = if (config.toc) List("--toc") else Nil
    val (opts, outF) = outFormat match {
      case OutputFormat.Pdf =>
        (tocOpt, OutputFormat.Tex)
      case _ =>
        (tocOpt, outFormat)
    }

    val outputFile = File.createTempFile("pandoc-", s"-typesetr.${outF.suffix}")
    val cmd =
      s"pandoc -f ${inF.suffix} -t ${outF.name} -o ${outputFile.getAbsolutePath} " +
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
   * @param origDoc postprocessed, pre-pandoc input file
   * @return a generic document generator.
   */
  def pandocPostprocessor(inFile: File, outFile: File, config: Config, docMeta: MetaFromDocument, origDoc: File): Either[String, pandoc.Writer] = {
    val (template, mediaExtractor) = config.inFormat match {
      case InputFormat.Odt =>
        (styles.StyleTemplate.odt(config.styleBase, config.style),
          new parsers.odt.OdtMediaExtractor(origDoc))
      // TODO: other templating formats
      case _ =>
        ???
    }
    config.outFormat match {
      case OutputFormat.Pdf =>
        Right(new LatexWriter(inFile, outFile, template, docMeta, true, mediaExtractor))
      case OutputFormat.Tex =>
        Right(new LatexWriter(inFile, outFile, template, docMeta, false, mediaExtractor))
      case format =>
        Left("No postprocessor for the $format format")
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
 * Output configuration
 */
abstract class ProcessingFileInfo {

  /**
   * Final output format, based on the input/output file
   */
  def format: OutputFormat

  /**
   * Desired target file
   */
  def outputFile: File

}

object ProcessingFileInfo {

  def apply(format: OutputFormat, outputFile: File): ProcessingFileInfo =
    FProcessingFile(format, outputFile)

  private case class FProcessingFile(format: OutputFormat,
                                     outputFile: File) extends ProcessingFileInfo

}

