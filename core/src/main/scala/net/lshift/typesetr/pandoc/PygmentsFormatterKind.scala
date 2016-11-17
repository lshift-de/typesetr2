package net.lshift.typesetr
package pandoc

/**
 * Supported code formatting output in the Pygments library
 */
sealed abstract class PygmentsFormatterKind

case object LatexFormatter extends PygmentsFormatterKind

case object HtmlFormatter extends PygmentsFormatterKind