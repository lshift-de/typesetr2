package net.lshift.typesetr
package styles

import scalaz.Tags.First
import scalaz.Scalaz._

case class LangLocalization(lang: Lang, subject: String)

object Localizations {

  /* Provide a translation for string `s`.
   * (American) English roundtrips:
   * >>> Lang('en').localize('Table of Contents')
   * u'Table of Contents'
   * >>> Lang('en-UK').localize('Table of Contents')
   * u'Table of Contents'

   * If no localization for a dialect is present, the localization for the
   * language as such is returned.
   * >>> Lang('de').localize('Table of Contents')
   * u'Inhaltsverzeichnis'
   * >>> Lang('de-CH').localize('Table of Contents') # fallback to 'de'
   * u'Inhaltsverzeichnis'

   * If no translation is present, fall back to English and warn:
   * >>> Lang('ms').localize('Table of Contents')
   * u'Table of Contents'
   */
  def localize(subject: String)(implicit lang: Lang): Option[String] = {
    def localize0(lang: Lang): Option[String] =
      for {
        entry <- translations.get(subject)
        translation <- entry.find(_.lang == lang)
      } yield translation.subject

    scalaz.Tag.unwrap(First(localize0(lang)) |+| First(localize0(lang.dialectFree)))
  }

  import Lang.de

  private val translations = Map(
    "Author" -> List(LangLocalization(de, "Autor")),
    "Title" -> List(LangLocalization(de, "Titel")),
    "Date" -> List(LangLocalization(de, "Datum")),
    "Short-title" -> List(LangLocalization(de, "Kurztitel")),
    "Subtitle" -> List(LangLocalization(de, "Untertitel")),
    "Abstract" -> List(LangLocalization(de, "Abstract")),
    "ISBN" -> List(LangLocalization(de, "ISBN")),
    "Printing" -> List(LangLocalization(de, "Druck")),
    "Confidential" -> List(LangLocalization(de, "Vertraulich")),
    "Draft" -> List(LangLocalization(de, "Entwurf")),
    "Version" -> List(LangLocalization(de, "Version")),
    "Project" -> List(LangLocalization(de, "Projekt")),
    "Client" -> List(LangLocalization(de, "Klient")),
    "Recipients" -> List(LangLocalization(de, "Empfänger")),
    "Recipient" -> List(LangLocalization(de, "Empfänger")),
    "Recipient address" -> List(LangLocalization(de, "Empfänger Adresse")),
    "Opening" -> List(LangLocalization(de, "Eröffnung")),
    "Subject" -> List(LangLocalization(de, "Betreff")),
    "Closing" -> List(LangLocalization(de, "Schluss")),
    "Signature" -> List(LangLocalization(de, "Unterschrift")),
    "Place" -> List(LangLocalization(de, "Ort")),
    "Keywords" -> List(LangLocalization(de, "Schlüsselwörter")),
    "Thanks" -> List(LangLocalization(de, "Danksagungen")),
    "Notice" -> List(LangLocalization(de, "Notiz")),
    "Affiliation" -> List(LangLocalization(de, "Affiliation")),
    "Terms & Conditions" -> List(LangLocalization(de, "AGB")),
    "Contents" -> List(LangLocalization(de, "Inhalt")),
    "Table of Contents" -> List(LangLocalization(de, "Inhaltsverzeichnis")),
    "Table of contents" -> List(LangLocalization(de, "Inhaltsverzeichnis")),
    "Start of Contents" -> List(LangLocalization(de, "Inhaltsanfang")),
    "yes" -> List(LangLocalization(de, "ja")),
    "no" -> List(LangLocalization(de, "nein")),
    "Cover image" -> List(LangLocalization(de, "Umschlagsabbildung")),
    "Logo" -> List(LangLocalization(de, "Logo")),
    "Bibliography" -> List(LangLocalization(de, "Bibliographie")),
    "Bibliography-preamble" -> List(LangLocalization(de, "Bibliographiepreambel")),
    "Language" -> List(LangLocalization(de, "Sprache")),
    "Section-numbering-depth" -> List(LangLocalization(de, "Kapitelnummerierungstiefe")),
    "Table of contents depth" -> List(LangLocalization(de, "Inhaltsverzeichnisnummerierungstiefe")))

}

