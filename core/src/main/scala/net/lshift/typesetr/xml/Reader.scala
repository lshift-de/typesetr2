package net.lshift.typesetr.xml

import java.io.{ InputStream, File }

import scala.xml.Elem

trait Reader {
  def fromFile(file: File): Elem
  def fromStream(io: InputStream): Elem
}
