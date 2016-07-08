package net.lshift.typesetr.cmd

import scopt.Read

sealed abstract class OutputFormat

object OutputFormat {
  case object Tex extends OutputFormat
  case object Pdf extends OutputFormat
  case object Png extends OutputFormat
  case object Html extends OutputFormat
  case object EPub extends OutputFormat
  case object Meta extends OutputFormat
  case object Internal extends OutputFormat
  case object Pickle extends OutputFormat

  implicit def toOpt: Read[OutputFormat] = Read.reads { (name: String) =>
    name match {
      case OutputFormat(f) => f
      case other           => throw new IllegalArgumentException(s"Unknown format $other")
    }
  }

  def unapply(x: String): Option[OutputFormat] = x match {
    case "text"     => Some(Tex)
    case "pdf"      => Some(Pdf)
    case "png"      => Some(Png)
    case "html"     => Some(Html)
    case "epub"     => Some(EPub)
    case "meta"     => Some(Meta)
    case "internal" => Some(Internal)
    case "pickle"   => Some(Pickle)
    case _          => None
  }
}

