package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.cmd._

import java.io.File

import net.lshift.typesetr.pandoc.Markers
import net.lshift.typesetr.parsers
import net.lshift.typesetr.parsers.OdtParser
import net.lshift.typesetr.util.Logger
import net.lshift.typesetr.writers.odt.OdtWriter

class OdtOptimizerRunner(styleBase0: File) extends FileConfigUtils {
  def run(input: File): Either[String, File] = {
    implicit val config = defaultOdtConfig(input)
    implicit val logger = Logger(config.logging)

    // 1. Infer the input-specific parser
    val parser = new parsers.OdtParser()
    val writer = new OdtWriter(input)

    Markers.reset()

    lazy val optimizer = postprocessors.DefaultPostProcessor.fromConfig(parser.nodeConfig)

    // 2. Parse the document and produce an internal
    // representation of the document
    (for {
      parsedDoc <- parser.parse(input).right
      // 3. Optimize the document
      optimizedDoc <- Right(if (config.Yoptimize)
        optimizer.optimize(parsedDoc.root)(implicitly[Logger], parsedDoc.style)
      else
        parsedDoc.root).right
      //inferredMeta <- Right(optimizer.inferMeta(optimizedDoc)(parsedDoc.style)).right
      inferredMeta <- optimizer.inferMeta(optimizedDoc)(parsedDoc.style).right
      // 4. Store the optimized document
      odtOutputF <- writer.writeToFile(inferredMeta._1)(implicitly[Logger], config).
        toRight("Failed to save the optimized document to a file").right
    } yield odtOutputF)
  }

  private def defaultOdtConfig(inF: File): Config = {
    Config(
      styleBase = styleBase0,
      style = Style("lshfit", "pitch"),
      inFormat = InputFormat.Odt,
      outFormat = OutputFormat.Pdf, // doesn't matter much
      inFile = Some(inF))
  }

}
