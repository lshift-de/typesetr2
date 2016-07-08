package net.lshift.typesetr.util

import net.lshift.typesetr.cmd.LogLevel

abstract class Logger {
  def info(v: => String): Unit
  def warn(v: => String): Unit
  def setLevel(v: LogLevel): Logger
}

object Logger {
  def apply(level: LogLevel): Logger =
    new LoggerImpl().setLevel(level)

  class LoggerImpl extends Logger {
    private[this] var logLevel: LogLevel = LogLevel.Log

    def info(v: => String): Unit = ???

    def warn(v: => String): Unit = ???

    def setLevel(v: LogLevel): Logger = {
      logLevel = v
      this
    }
  }
}