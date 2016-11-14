package net.lshift.typesetr.pandoc

import java.io.File

import net.lshift.typesetr.cmd.{ Config, OutputFormat, InputFormat }

trait PandocPhase {

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

}
