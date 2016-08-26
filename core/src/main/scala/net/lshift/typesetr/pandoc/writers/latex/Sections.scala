package net.lshift.typesetr.pandoc.writers.latex

sealed abstract class Sections {
  override def toString = super.toString.toLowerCase
}

case object Part extends Sections
case object Chapter extends Sections
case object Section extends Sections
case object Subsection extends Sections
case object Subsubsection extends Sections
case object Paragraph extends Sections
case object Subparagraph extends Sections
case object Subsubparagraph extends Sections
