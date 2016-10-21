package net.lshift.typesetr

import java.io.File

abstract class Differ {
  /**
   * Compares two files and returns a difference, if at all.
   *
   * @return a non-empty string describing the difference,
   *         or an empty option if they are the same.
   */
  def diff(spec: File, output: File): Option[String]
}
