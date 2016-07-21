package net.lshift.typesetr.xml

import java.io.{ File, InputStream }

import scala.xml.Elem

class ScalaReader extends Reader {
  override def fromFile(file: File): Elem = ???

  override def fromStream(io: InputStream): Elem = ???
}
