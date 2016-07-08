package net.lshift.typesetr

import java.io.File

abstract class StyleTemplate {
  def style: cmd.Style
  def exists: Boolean
}

object StyleTemplate {
  final val metaData: String = "metadata.yml"

  def apply(style: cmd.Style, baseDir: File): StyleTemplate =
    new StyleTemplateImpl(style, baseDir)
}

class StyleTemplateImpl(val style: cmd.Style, baseDir: File)
  extends StyleTemplate {

  def exists: Boolean = {
    val f = new File(baseDir.getAbsolutePath + File.separator +
      style + File.separator + StyleTemplate.metaData)
    f.exists() && f.isFile()
  }

}