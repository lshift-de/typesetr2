package net.lshift.typesetr
package parsers.odt.styles

import net.lshift.typesetr.parsers.styles.StylePropKey
import shapeless.HMap

/*
 * Defines the relation between the style property and how
 * information about should be extracted from the properties
 * nodes.
 */
object StylePropertyExtractors {

  class StyleProp2Extractor[K, V]

  object StyleProp2Extractor {
    implicit def fromPropKey2ExtrMap[T <: StylePropKey]: StyleProp2Extractor[T, PropertiesExtractorFactory] =
      new StyleProp2Extractor[T, PropertiesExtractorFactory]
  }

  val map = HMap[StyleProp2Extractor](
    OdtStylePropKeys.FontFamily      -> PropertiesExtractor.text,
    OdtStylePropKeys.FontSeize       -> PropertiesExtractor.text,
    OdtStylePropKeys.FontWeight      -> PropertiesExtractor.text,
    OdtStylePropKeys.FontStyleProp   -> PropertiesExtractor.text,

    OdtStylePropKeys.Color           -> PropertiesExtractor.mixed,
    OdtStylePropKeys.BackgroundColor -> PropertiesExtractor.mixed,

    OdtStylePropKeys.Underline       -> PropertiesExtractor.text,
    OdtStylePropKeys.LineThrough     -> PropertiesExtractor.text,
    OdtStylePropKeys.TextPosition    -> PropertiesExtractor.text,

    OdtStylePropKeys.TextAlign       -> PropertiesExtractor.paragraph,
    OdtStylePropKeys.LineHeight      -> PropertiesExtractor.paragraph,
    OdtStylePropKeys.MarginLeft      -> PropertiesExtractor.paragraph,
    OdtStylePropKeys.ParBreak        -> PropertiesExtractor.paragraph,
    OdtStylePropKeys.TextIndent      -> PropertiesExtractor.paragraph,

    OdtStylePropKeys.ColWidth        -> PropertiesExtractor.paragraph,
    OdtStylePropKeys.MinHeight       -> PropertiesExtractor.paragraph

  )
}
