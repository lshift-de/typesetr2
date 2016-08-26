package net.lshift.typesetr
package styles

import cmd.OutputFormat

import io.circe.Json

import java.io.File

abstract class StyleTemplate {
  import StyleTemplate._

  def path: File
  def basePath: File

  def template: Option[File]

  protected def latexIncPath: File = path \ latexPrefix \ includePrefix
  protected def latexTemplPath: File = path \ latexPrefix \ templatePrefix \ templateFName
  protected def texPath: File = path \ latexPrefix

  def section: Option[Int] = {
    val secF = path \ latexPrefix \ sectionFile
    if (secF.exists) readSectionFromFile(secF)
    else Some(1)
  }

  lazy val metaSchema: Option[MetaSchema] = {
    val f = path \ metaFile
    if (f.exists()) MetaSchema.apply(f)
    else None
  }

  def copyFilesTo(target: File): Boolean = {
    // 1. Include shared files
    // 2. Include style-specific files
    val shared = basePath \ sharedPrefix \ latexPrefix \ includePrefix

    (latexIncPath.exists() && shared.exists()) &&
      (shared.copyTo(target, withInitialDir = true) &&
        latexIncPath.copyTo(target, withInitialDir = true))
  }

  private def readSectionFromFile(f: File): Option[Int] =
    f.loadFile() flatMap { content =>
      try { Some(content.toInt) } catch { case _: NumberFormatException => None }
    }

  private def isFormatSupported(out: OutputFormat): Boolean = {
    out match {
      case OutputFormat.Tex => (path \ latexPrefix).exists()
      case OutputFormat.Pdf => (path \ latexPrefix).exists()
      case _                => false // TODO
    }
  }

}

object StyleTemplate {

  def gdoc(base: File, name: String, meta: Json): StyleTemplate =
    ???

  def odt(base: File, name: String): StyleTemplate =
    OdtStyleTemplate(base \ name, base)

  def html(base: File, name: String): StyleTemplate =
    ???

  protected final val latexPrefix = "latex"
  protected final val sharedPrefix = "shared"
  protected final val includePrefix = "include"
  protected final val texPrefix = "tex"
  protected final val templatePrefix = "template"
  protected final val templateFName = "template.tex"
  protected final def falback(f: File): File = f \ "shared" \ "fallbacks"
  protected final val sectionFile = "section-corresponds-to"
  protected final val metaFile = "metadata.yml"

}

case class GDocStyleTemplate(path: File, basePath: File, gdoc_meta: Json) extends StyleTemplate {
  def template: Option[File] = {
    ???
  }
}

case class OdtStyleTemplate(path: File, basePath: File) extends StyleTemplate {
  def template: Option[File] = Some(latexTemplPath)
}

case class HtmlStyleTemplate(path: File, basePath: File) extends StyleTemplate {
  def template: Option[File] = ???
}
