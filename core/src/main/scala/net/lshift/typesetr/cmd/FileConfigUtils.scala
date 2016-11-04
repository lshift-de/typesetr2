package net.lshift.typesetr.cmd

import java.io.{ IOException, PrintWriter, File }

import net.lshift.typesetr.util.Logger
import org.apache.commons.io.FilenameUtils

trait FileConfigUtils {

  def retrieveInputFile(config: Config): Either[String, File] = {
    config.inFile match {
      case Some(file) => Right(file)
      case None =>

        // Read from a standard input and save to
        // a temporary file
        val conts = scala.io.Source.stdin.getLines().mkString(System.lineSeparator)

        val kind = conts match {
          case InputFormat(format) =>
            Right(format)
          case _ =>
            Left("Unable to infer the type of the input from the input format")
        }

        try {
          for {
            k <- kind.right
          } yield {
            val f = java.io.File.createTempFile("typeseter", k.suffix)
            val pw = new PrintWriter(f)
            pw.write(conts)
            pw.close
            f
          }
        } catch {
          case _: IOException =>
            Left("Unable to read the input document from the input stream")
        }
    }
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

        Right(ProcessingFileInfo(format, fOut))
      case _ =>
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
