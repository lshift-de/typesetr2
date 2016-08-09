package net.lshift.typesetr.cmd

import scopt.Read

sealed abstract class InputFormat {
  val suffix: String
}

object InputFormat {
  case object Odt extends InputFormat { val suffix = "odt" }
  case object Docx extends InputFormat { val suffix = "docx" }
  case object Markdown extends InputFormat { val suffix = "md" }
  case object Html extends InputFormat { val suffix = "html" }

  implicit def toOpt: Read[InputFormat] = Read.reads { (name: String) =>
    name match {
      case InputFormat(f) => f
      case other =>
        throw new IllegalArgumentException(s"Unknown input format $other")
    }
  }

  def unapply(format: String): Option[InputFormat] = format match {
    case Odt.suffix      => Some(Odt)
    case Docx.suffix     => Some(Docx)
    case Markdown.suffix => Some(Markdown)
    case Html.suffix     => Some(Html)
    case _               => None
  }
}
