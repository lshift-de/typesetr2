package net.lshift.typesetr.util

import java.io.File

abstract class MediaExtractor {
  def copyMediaTo(target: File): Boolean
}
