package net.lshift.typesetr.cmd

import java.io.File

import scopt.Read

sealed abstract class GDocIdentifier

object GDocIdentifier {

  case class GDocId(id: String) extends GDocIdentifier

  case class GDocPath(file: File) extends GDocIdentifier

  implicit def toRead[T]: Read[GDocIdentifier] = Read.reads { (v: String) =>
    val f = new File(v)
    if (f.exists()) GDocPath(f)
    else GDocId(v) // TODO: verify the pattern of GUID
  }
}