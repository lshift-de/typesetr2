package net.lshift.typesetr.cmd

import scopt.Read

/**
 * Representation of the output format
 *
 * @param suffix - a standard file extension of the output format
 * @param name - name of the output format
 */
sealed abstract class OutputFormat(val suffix: String, val name: String)

object OutputFormat {

  case object Tex extends OutputFormat("tex", "latex")
  case object Pdf extends OutputFormat("pdf", "pdf")
  case object Png extends OutputFormat("png", "png")
  case object Html extends OutputFormat("html", "html")
  case object EPub extends OutputFormat("epub", "epub")
  case object Meta extends OutputFormat("meta", "meta")
  case object Internal extends OutputFormat("internal", "internal")

  implicit def toOpt: Read[OutputFormat] = Read.reads { (name: String) =>
    name match {
      case OutputFormat(f) => f
      case other =>
        throw new IllegalArgumentException(s"Unknown output format $other")
    }
  }

  def unapply(x: String): Option[OutputFormat] = x match {
    case "tex"      => Some(Tex)
    case "pdf"      => Some(Pdf)
    case "png"      => Some(Png)
    case "html"     => Some(Html)
    case "epub"     => Some(EPub)
    case "meta"     => Some(Meta)
    case "internal" => Some(Internal)
    case _          => None
  }
}

