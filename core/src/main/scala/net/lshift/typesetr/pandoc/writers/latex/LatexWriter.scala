package net.lshift.typesetr
package pandoc.writers.latex

import java.io.{ Writer => IOWriter, _ }
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import cmd.{ LogLevel, Config }
import pandoc.Writer
import pandoc.writers._
import styles.{ MetaEntry, MetaSchema, MetaKey, Lang }
import util.Logger
import org.apache.commons.io.FileUtils

import sys.process.Process

import scala.language.postfixOps

import XMPMeta.lang

class LatexWriter(from: File, target: File, template: styles.StyleTemplate, generatePdf: Boolean) extends Writer {
  import LatexWriter._

  def write(config: Config)(implicit logger: Logger): Unit = {
    val targetLatexF = for {
      meta <- template.metaSchema
      body <- from.loadFile()
      templFile <- template.template
      templBody <- templFile.loadFile()

    } yield {
      val langProp = meta.getKey(lang).map(_.raw).flatMap(Lang.apply).getOrElse(Lang.default)
      val xmpFields = xmpMeta(meta)
      val babelHeader = BABEL_HEADER(langProp.toBabel)
      val latexHead = makeLatexHead(meta)

      // FIXME: inefficient, avoid generating complete strings
      //        every single time
      val finalContent =
        templBody.
          replaceFirst(SectionBabel, babelHeader).
          replaceFirst(SectionHead, latexHead.mkString("\n")).
          replaceFirst(SectionMeta, xmpFields.mkString("\n")).
          replaceAllLiterally(SectionBody, body)
      // TODO: include bibliography

      val f =
        if (generatePdf) File.createTempFile("writer-", ".tex")
        else target

      f.writeToFile(finalContent)
      f
    }

    for {
      fromF <- targetLatexF if generatePdf
    } yield {
      // Pdf convert it, if necessary
      // 1. Create a single directory where all stuff is going to be compiled from
      // 2. Copy over styles, includes etc
      // 3. Compile using xelatex
      val tmpDir = File.createTempFile("pdf", "typesetr")
      tmpDir.delete()
      tmpDir.mkdirs()
      template.copyFilesTo(tmpDir)

      val latexOpts = List(
        if (config.logging == LogLevel.Debug) "" else "-silent",
        "-xelatex", "-pdf").mkString(" ")

      val cmd = Process(Seq("bash", "-c", s"""latexmk $latexOpts ${fromF}"""),
        Some(tmpDir), "TEXINPUTS" -> ".:./include:", "BSTINPUTS" -> "./include:")

      cmd !

      if (target.exists())
        logger.info(s"Target file already exists. Overriding.")
      val tmpPdf = tmpDir \ (fromF.getName.stripSuffix(".tex") + ".pdf")
      tmpPdf.moveTo(target)

      ()
    }

  }

  /*
   *  Create XMP metadata for pdf (via hyperxmp.sty).
   *
   *
   *  Note that generating well structured pdf output with latex is a fool's
   *  errand, so this has some shortcomings:
   *
   *  - dc:language  should be of type 'bag'.
   *  - XMP and info entries aren't synched for
   *    'xmp:CreateDate'/'CreationDate' and 'pdf:Producer'/'Producer'.
   *  - For PDF/A we'd also need 'pdfaid:part' and 'pdfaid:conformance', on
   *    first sight it looks like hypermp.sty takes a hardcoded guess, with
   *    two possible outcomes: nothing or PDF/A-1b.
   *
   *  In an ideal world we'd probably only create valid PDF/A-2u or PDF/X
   *  documents, but both seem pretty much impossible to achieve from latex
   *  directly, with even the trivial metadata stuff above being a pain and
   *  then we'd also need to deal, at the very least, with ICC Profiles and
   *  unicode mappings (already somewhat painful for reasons of historical
   *  baggage in PDF and the font standards it supports and really horrible
   *  in latex because e.g. of issues with zero-width glyphs and math
   *  characters without unicode equivalent). PDF/A-2a would also require
   *  tagging.
   *
  */
  private def xmpMeta(meta: MetaSchema): List[String] = {
    val xmps = XMPMeta.all().flatMap { key =>
      meta.getKey(key).map((key, _))
    } map { case (key, v) => v.xmpEncoded(key) }
    "pdfcreator={Typesetr}" :: xmps
  }

  private def makeLatexHead(meta: MetaSchema)(implicit tools: LatexTools): Iterable[String] = {

    for {
      (k, v) <- meta
      lV <- LatexMetaVar(k, v)
    } yield {
      tools.cmd(metaHeadCmd, Nil, List("\\\\" + lV.fullName, lV.value))
      // TODO: commands
      // TODO: url defs
    }
  }

}

object LatexWriter {

  private final val metaHeadCmd = "newcommand"

  private final val SECTION_COMMANDS =
    List(Part, Chapter, Section, Subsection,
      Subsubsection, Paragraph, Subparagraph,
      Subsubparagraph)

  // `\def\languageshorthands#1{}` = nuke languageshorthands to prevent babel
  // from extending the set of escape sequences in a language-specific fashion
  // e.g. with language ngerman `"a` -> `ä` etc. breaking our escaping
  // Note: cannot use multiline stringinterpolation because
  // it confuses the beginning of the line in |\\
  private final val BABEL_HEADER = (lang: String) =>
    s"""|\\\\usepackage[${lang}]{babel}
        |\\\\usepackage[${lang}]{isodate}
        |\\\\def\\\\languageshorthands#1{}""".stripMargin

  private final val SectionBabel = "INTERPOLATEBABEL"
  private final val SectionHead = "INTERPOLATEHEAD"
  private final val SectionMeta = "INTERPOLATEMETA"
  private final val SectionBody = "INTERPOLATEBODY"

}

/**
 * Latex-converted meta info
 *
 * Key and value for the meta information of the document
 *
 * @param name - typesetr-specific name of the key
 * @param orig - value of the individual meta information
 */
class LatexMetaVar(name: String, orig: MetaEntry) {
  def fullName: String = LatexMetaVar.prefix + name

  def value: String = orig.toLatex
}

object LatexMetaVar {

  private final val prefix: String = "tystr"

  def apply(metaVar: MetaKey, metaEntry: MetaEntry): Option[LatexMetaVar] =
    metaVar.latexify.map(new LatexMetaVar(_, metaEntry))

}

