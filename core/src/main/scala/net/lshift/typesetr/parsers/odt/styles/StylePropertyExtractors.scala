package net.lshift.typesetr
package parsers.odt.styles

import shapeless.HMap

/*
 * Defines the relation between the style property and how
 * information about should be extracted from the properties
 * nodes.
 */
object StylePropertyExtractors {

  class StyleProp2Extractor[K, V]

  object StyleProp2Extractor {
    implicit def fromPropKey2ExtrMap[T <: StylePropKey]: StyleProp2Extractor[T, PropertiesExtractorBuilder] =
      new StyleProp2Extractor[T, PropertiesExtractorBuilder]
  }

  val map = HMap[StyleProp2Extractor](
    StylePropKey.FontFamily -> PropertiesExtractor.text,
    StylePropKey.FontSeize -> PropertiesExtractor.text,
    StylePropKey.FontWeight -> PropertiesExtractor.text,
    StylePropKey.FontStyleProp -> PropertiesExtractor.text,

    StylePropKey.Color -> PropertiesExtractor.mixed,
    StylePropKey.BackgroundColor -> PropertiesExtractor.mixed,

    StylePropKey.Underline -> PropertiesExtractor.text,
    StylePropKey.LineThrough -> PropertiesExtractor.text,
    StylePropKey.TextPosition -> PropertiesExtractor.text,

    StylePropKey.TextAlign -> PropertiesExtractor.paragraph,
    StylePropKey.LineHeight -> PropertiesExtractor.paragraph,
    StylePropKey.MarginLeft -> PropertiesExtractor.paragraph,
    StylePropKey.ParBreak -> PropertiesExtractor.paragraph,
    StylePropKey.TextIndent -> PropertiesExtractor.paragraph,

    StylePropKey.ColWidth -> PropertiesExtractor.paragraph,
    StylePropKey.MinHeight -> PropertiesExtractor.paragraph

  )
}
