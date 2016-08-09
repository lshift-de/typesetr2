package net.lshift.typesetr.cmd

import scopt.Read

sealed abstract class LogLevel {
  val repr: String
  def lvl: Int
}

object LogLevel {

  case object NoLog extends LogLevel {
    val repr: String = "no-log"
    def lvl: Int = 0

  }
  case object Log extends LogLevel {
    val repr: String = "log"
    def lvl: Int = 1
  }
  case object Raise extends LogLevel {
    val repr: String = "raise"
    def lvl: Int = 2
  }
  case object Debug extends LogLevel {
    val repr: String = "debug"
    def lvl: Int = 3
  }

  implicit def toLogLeveRead: Read[LogLevel] = Read.reads { (v: String) =>
    v match {
      case NoLog.repr => NoLog
      case Log.repr   => Log
      case Raise.repr => Raise
      case Debug.repr => Debug
      case _ =>
        throw new IllegalArgumentException(s"Invalid log format $v")
    }
  }

}
