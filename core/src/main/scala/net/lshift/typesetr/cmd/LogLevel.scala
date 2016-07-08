package net.lshift.typesetr.cmd

import scopt.Read

sealed abstract class LogLevel {
  val repr: String
}

object LogLevel {

  case object Log extends LogLevel {
    val repr: String = "log"
  }
  case object Raise extends LogLevel {
    val repr: String = "raise"
  }
  case object Debug extends LogLevel {
    val repr: String = "debug"
  }

  implicit def toLogLeveRead: Read[LogLevel] = Read.reads { (v: String) =>
    v match {
      case Log.repr   => Log
      case Raise.repr => Raise
      case Debug.repr => Debug
      case _ =>
        throw new IllegalArgumentException(s"Invalid style format $v")
    }
  }

}
