package net.lshift.typesetr.parsers.odt

import java.io._
import java.nio.file.Files.copy
import java.nio.file.Path
import java.nio.file.Paths.get
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.util.Date
import java.util.zip._

import scala.collection.mutable
import scala.language.implicitConversions

class OdtFile(mmap: Map[String, File]) {
  import OdtFile._

  def content: Option[File] = mmap.get(CONTENT_F)
  def style: Option[File] = mmap.get(STYLES_F)

  def copyToDir(dir: File): Boolean =
    mmap forall {
      case (key, f) =>
        f.mkdirs()
        try {
          val targetF = new File(dir + File.separator + key)
          targetF.mkdirs()
          copy(f, targetF, REPLACE_EXISTING)
          true
        } catch {
          case ex: IOException =>
            ex.printStackTrace()
            false
        }
    }
}

object OdtFile {
  private final val CONTENT_F = "content.xml"
  private final val STYLES_F = "styles.xml"

  private final val MIMETYPE = "mimetype"

  def fromMap(mmap: Map[String, File]): Option[OdtFile] =
    for {
      c <- mmap.get(CONTENT_F)
      s <- mmap.get(STYLES_F)
    } yield new OdtFile(mmap)

  class OdtFileOps(val f: File) extends AnyVal {
    def unpack(): Option[OdtFile] = {
      if (f.exists() && f.getName.endsWith(".odt")) {
        val dir = File.createTempFile("typesetr", "odts")
        dir.delete()
        dir.mkdir()

        val buffer = new Array[Byte](1024)

        val zis = new ZipInputStream(new FileInputStream(f))
        //get the zipped file list entry
        var ze: ZipEntry = zis.getNextEntry()

        var mmap: Map[String, File] = Map()

        while (ze != null) {
          val newFile = new File(dir + File.separator + ze.getName)
          mmap = mmap + (ze.getName -> newFile)

          if (!ze.isDirectory) {

            ze.getName.split(File.separator).toList match {
              case _ :: Nil =>
              // no intermediate directories
              case dirs =>
                (new File(dir + File.separator + dirs.init.mkString(File.separator))).mkdirs()
            }

            val fos = new FileOutputStream(newFile)

            var len: Int = zis.read(buffer)

            while (len > 0) {
              fos.write(buffer, 0, len)
              len = zis.read(buffer)
            }

            fos.close()

          }

          ze = zis.getNextEntry()
        }

        zis.closeEntry()
        zis.close()

        OdtFile.fromMap(mmap)
      } else None
    }

    def pack(target: FileOutputStream, origFile: OdtFile): Boolean = {
      // 1) copy all files from the origFile to target
      // 2) rename contentXML to content.xml
      // 3) zip and wrap in odt
      if (f.exists()) {
        try {
          val dir = File.createTempFile("typesetr-optimized", "odts")
          dir.delete()
          dir.mkdir()

          origFile.copyToDir(dir)
          copy(f, new File(dir + File.separator + CONTENT_F), REPLACE_EXISTING)
          val (files1, mimeFile) = dir.listFiles().toList.span(_.getName != MIMETYPE)

          // Entries must not contain a leading backslash
          val pre = dir.getAbsolutePath + File.separator

          val q = new mutable.Queue[File]()
          q.enqueue(mimeFile: _*)
          q.enqueue(files1: _*)

          val zip = new ZipOutputStream(target)

          zip.setMethod(ZipOutputStream.DEFLATED)

          while (q.nonEmpty) {

            val buffer = new Array[Byte](1024)
            val entry = q.dequeue()

            if (entry.isDirectory)
              q.enqueue(entry.listFiles: _*)
            else {
              val fIn = new FileInputStream(entry)
              val zipEntryName = entry.getAbsolutePath.stripPrefix(pre)
              val zipEntry = new ZipEntry(zipEntryName)
              var length = fIn.read(buffer)

              // According to spec mimetype must not be
              // be compressed so we have to calculate manually
              // all the necessary info
              if (entry.getName == MIMETYPE) {
                zipEntry.setMethod(ZipEntry.STORED)

                checkSum(entry) foreach {
                  case (c1, c2) =>
                    zipEntry.setSize(c1)
                    zipEntry.setCompressedSize(c1)
                    zipEntry.setTime(new Date().getTime)
                    zipEntry.setCrc(c2)
                }

              } else zipEntry.setMethod(ZipEntry.DEFLATED)

              zip.putNextEntry(zipEntry)

              while (length > 0) {
                zip.write(buffer, 0, length)
                length = fIn.read(buffer)
              }
              fIn.close()
            }
            zip.closeEntry()
          }
          zip.close()

          true
        } catch {
          case ex: IOException =>
            ex.printStackTrace()
            false
        }
      } else false
    }
  }

  private def checkSum(file: File): Option[(Long, Long)] = {
    try {
      // Computer CRC32 checksum
      val cis = new CheckedInputStream(new FileInputStream(file), new CRC32())
      val buf = new Array[Byte](1024)
      var read = cis.read(buf)
      while (read >= 0) read = cis.read(buf)
      cis.close()
      Some((file.length, cis.getChecksum.getValue))
    } catch {
      case _: IOException => None
    }
  }

  private[parsers] implicit def toPath(f: File): Path = get(f.getAbsolutePath)
  private[parsers] implicit def toPath(f: String): Path = get(f)

}
