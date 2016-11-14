package net.lshift.typesetr

import java.io.File

abstract class Differ[T] {
  /**
   * Compares two files and returns a difference, if at all.
   *
   * @return a non-empty string describing the difference,
   *         or an empty option if they are the same.
   */
  def diff(spec: T, output: T): Option[String]
}
