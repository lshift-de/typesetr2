package net.lshift.typesetr
package util

import java.io.{ IOException, File }
import java.nio.charset.StandardCharsets
import java.nio.file.Files.copy
import java.nio.file.{ Paths, Files, Path }
import java.nio.file.Paths._
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

import org.apache.commons.io.FileUtils

import scala.collection.mutable

import scala.language.implicitConversions

// File utilities available through method extensions
object IO {

  class FileOps(val x: File) extends AnyVal {

    /**
     * Create a new file/path using a suffix
     *
     * @param f - suffix that is appended to the original file name
     * @return - a modified file path
     */
    def \(f: String): File = {
      assert(x.isDirectory, s"$x is not a directory")
      new File(x + File.separator + f)
    }

    /**
     * Copy all files of the `x` directory into the `target` directory..
     * If `x` is a file, then it is simply copied over to the `target` directory.
     *
     * @param target - target directory where files are being copied over
     * @param withInitialDir - if true, then the whole directory structure is copied over
     * @return - true if copying was successful, false otherwise.
     */
    def copyTo(target: File, withInitialDir: Boolean = false): Boolean = {
      if (!target.exists()) target.mkdirs()

      val pre =
        if (withInitialDir) x.getAbsolutePath.stripSuffix(x.getName)
        else x.getAbsolutePath + File.separator

      val queue = new mutable.Queue[File]()
      if (!x.isDirectory)
        queue.enqueue(x)
      else
        queue.enqueue(x.listFiles(): _*)

      try {
        while (queue.nonEmpty) {
          val f = queue.dequeue()
          if (f.isDirectory) {
            queue.enqueue(f.listFiles(): _*)
          } else {
            val fName = f.getAbsolutePath.stripPrefix(pre)
            val targetF = target \ fName
            targetF.mkdirs()
            copy(f, targetF, REPLACE_EXISTING)
          }
        }
        true
      } catch {
        case ex: IOException =>
          ex.printStackTrace()
          false
      }

    }

    /**
     * Rename file `x` to `target`
     * @param target - the file to rename to
     * @return - true, if moving the file was successful, false otherwise
     */
    def moveTo(target: File): Boolean =
      try {
        if (target.exists && target.isFile) target.delete()
        Files.move(x, target)
        true
      } catch { case ex: IOException => ex.printStackTrace(); false }

    /**
     * Loads the contents of the file `x` into a string
     * @return - a non-empty value if reading the file was successful
     */
    def loadFile(): Option[String] = {
      try {
        Some(FileUtils.readFileToString(x, StandardCharsets.UTF_8))
      } catch { case _: IOException => None }
    }

    /**
     * Write string value into a file `x`
     *
     * @param content - string value to store in a file
     * @return - true if writing to the file was successful, false otherwise
     */
    def writeToFile(content: String): Boolean = {
      try {
        Files.write(x, content.getBytes(StandardCharsets.UTF_8))
        true
      } catch { case _: IOException => false }
    }

    def deleteDirectory(): Boolean = {
      FileUtils.deleteDirectory(x)
      true
    }

    private[util] implicit def toPath(f: File): Path = get(f.getAbsolutePath)

  }
}
