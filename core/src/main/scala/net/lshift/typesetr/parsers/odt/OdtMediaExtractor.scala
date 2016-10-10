package net.lshift.typesetr
package parsers
package odt

import util.MediaExtractor
import java.io.File

class OdtMediaExtractor(odtFile: File) extends MediaExtractor {
  import OdtMediaExtractor._

  def copyMediaTo(targetDir: File): Boolean =
    (for {
      (odtFileMap, odtDir) <- odtFile.unpack()
    } yield {
      odtFileMap.copyToDir(targetDir, isMediaFile _)
    }).getOrElse(false)

  private def isMediaFile(file: File): Boolean =
    file.getParentFile.getName == MediaDir

}

object OdtMediaExtractor {

  private final val MediaDir = "Pictures"

}
