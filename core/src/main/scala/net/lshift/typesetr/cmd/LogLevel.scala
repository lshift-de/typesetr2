package net.lshift.typesetr.cmd

import scopt.Read

sealed abstract class LogLevel(val name: String, val lvl: Int)

object LogLevel {

  case object NoLog extends LogLevel("none", 0)

  case object Log extends LogLevel("log", 1)

  case object Raise extends LogLevel("raise", 2)

  case object Debug extends LogLevel("debug", 3)

  implicit def toLogLeveRead: Read[LogLevel] = Read.reads { (v: String) =>
    v match {
      case NoLog.`name` => NoLog
      case Log.`name`   => Log
      case Raise.`name` => Raise
      case Debug.`name` => Debug
      case _ =>
        throw new IllegalArgumentException(s"Invalid log format $v")
    }
  }

}
