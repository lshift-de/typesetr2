package net.lshift.typesetr.cmd

import scopt.Read

/**
 * Representation of the compression-type of the file
 */
sealed abstract class CompressKind

object CompressKind {
  case object Zip extends CompressKind {
    val repr: String = toString.toLowerCase()
  }

  implicit def toStyleRead: Read[CompressKind] = Read.reads { (v: String) =>
    v match {
      case Zip.repr => Zip
      case _ =>
        throw new IllegalArgumentException(s"Invalid compression format $v")
    }
  }
}

