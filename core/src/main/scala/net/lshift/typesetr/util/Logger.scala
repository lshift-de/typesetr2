package net.lshift.typesetr.util

import net.lshift.typesetr.cmd.LogLevel

abstract class Logger {
  def info(v: => String): Unit
  def warn(v: => String): Unit
  def fail(v: => String): Unit
  def setLevel(v: LogLevel): Logger
}

object Logger {
  def apply(level: LogLevel): Logger =
    new LoggerImpl().setLevel(level)

  class LoggerImpl extends Logger {
    private[this] var logLevel: LogLevel = LogLevel.NoLog

    def info(v: => String): Unit =
      if (logLevel.lvl > 0)
        println(s"[info] $v")

    def fail(v: => String): Unit =
      println(s"[error] $v")

    def warn(v: => String): Unit =
      if (logLevel.lvl > 1)
        println(s"[info] $v")

    def debug(v: => String): Unit =
      if (logLevel.lvl > 1)
        println("s[debug] $v")

    def setLevel(v: LogLevel): Logger = {
      logLevel = v
      this
    }
  }
}