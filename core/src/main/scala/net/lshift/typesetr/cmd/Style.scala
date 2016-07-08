package net.lshift.typesetr.cmd

import scopt.Read
import java.io.File

case class Style(base: String, tpe: String) {
  override def toString = s"$base/$tpe"
}

object Style {
  def default(): Style = Style("typesetr", "report-pitch")

  implicit def toStyleRead: Read[Style] = Read.reads { (v: String) =>
    v.split('/').toList match {
      case style :: tpe :: Nil =>
        // Ensure that the provided style exists
        Style(style, tpe)
      case _ => throw new IllegalArgumentException(s"Invalid style format $v")
    }
  }
}
