package net.lshift.typesetr
package styles

/**
 * A representation of the language value
 *
 * @param code - iso639 compatible language code
 * @param fullName - babel version of the language
 * @param lang0 - dialect-free version of the language
 */
case class Lang(code: String, fullName: String)(lang0: => Lang) {
  def toBabel: String = fullName
  def dialectFree: Lang = lang0
}

object Lang {

  private[styles] lazy val af: Lang = Lang("af", "afrikaans")(af)
  private[styles] lazy val id: Lang = Lang("id", "indonesian")(id)
  private[styles] lazy val ms: Lang = Lang("ms", "malay")(ms)
  private[styles] lazy val bg: Lang = Lang("bg", "bulgarian")(bg)
  private[styles] lazy val ca: Lang = Lang("ca", "catalan")(ca)
  private[styles] lazy val hr: Lang = Lang("hr", "croatian")(hr)
  private[styles] lazy val cs: Lang = Lang("cs", "czech")(cs)
  private[styles] lazy val da: Lang = Lang("da", "danish")(da)
  private[styles] lazy val nl: Lang = Lang("nl", "dutch")(nl)
  private[styles] lazy val en: Lang = Lang("en", "american")(en)
  private[styles] lazy val enUS: Lang = Lang("en-US", "american")(en)
  private[styles] lazy val enUK: Lang = Lang("en-UK", "british")(en)
  private[styles] lazy val enCA: Lang = Lang("en-CA", "canadian")(en)
  private[styles] lazy val enAU: Lang = Lang("en-AU", "australian")(en)
  private[styles] lazy val enNZ: Lang = Lang("en-NZ", "newzealand")(en)
  private[styles] lazy val et: Lang = Lang("et", "estonian")(et)
  private[styles] lazy val fi: Lang = Lang("fi", "finnish")(fi)
  private[styles] lazy val fr: Lang = Lang("fr", "french")(fr) // french appears to be a better default, but breaks w/ AP)(fr)
  private[styles] lazy val frFR: Lang = Lang("fr-FR", "french")(fr)
  private[styles] lazy val frCA: Lang = Lang("fr-CA", "canadien")(fr)
  private[styles] lazy val frCH: Lang = Lang("fr-CH", "french")(fr)
  private[styles] lazy val gl: Lang = Lang("gl", "galician")(gl)
  private[styles] lazy val de: Lang = Lang("de", "ngerman")(de) // reforme
  private[styles] lazy val deDE: Lang = Lang("de-DE", "ngerman")(de) // reforme
  private[styles] lazy val deAT: Lang = Lang("de-AT", "naustrian")(de) // reforme
  private[styles] lazy val deCH: Lang = Lang("de-CH", "ngerman")(de) // no swiss support in babe
  private[styles] lazy val el: Lang = Lang("el", "greek")(el)
  private[styles] lazy val he: Lang = Lang("he", "hebrew")(he)
  private[styles] lazy val hu: Lang = Lang("hu", "hungarian")(hu)
  private[styles] lazy val is: Lang = Lang("is", "icelandic")(is)
  private[styles] lazy val ga: Lang = Lang("ga", "irish")(ga)
  private[styles] lazy val it: Lang = Lang("it", "italian")(it)
  private[styles] lazy val la: Lang = Lang("la", "latin")(la)
  private[styles] lazy val no: Lang = Lang("no", "norsk")(no)
  private[styles] lazy val pl: Lang = Lang("pl", "polish")(pl)
  private[styles] lazy val pt: Lang = Lang("pt", "portuguese")(pt)
  private[styles] lazy val ptBR: Lang = Lang("pt-BR", "brazilian")(pt)
  private[styles] lazy val ro: Lang = Lang("ro", "romanian")(ro)
  private[styles] lazy val ru: Lang = Lang("ru", "russian")(ru)
  private[styles] lazy val es: Lang = Lang("es", "spanish")(es)
  private[styles] lazy val sk: Lang = Lang("sk", "slovak")(sk)
  private[styles] lazy val sl: Lang = Lang("sl", "slovene")(sl)
  private[styles] lazy val sv: Lang = Lang("sv", "swedish")(sv)
  private[styles] lazy val sr: Lang = Lang("sr", "serbian")(sr)
  private[styles] lazy val tr: Lang = Lang("tr", "turkish")(tr)
  private[styles] lazy val uk: Lang = Lang("uk", "ukrainian")(uk)

  private final val LangDialect = "([a-z][a-z])_([a-z]*)".r

  def apply(code: String): Option[Lang] = code match {
    case LangDialect(main, dialect) =>
      ISO_LANGS.get(s"$main-$dialect")
    case code =>
      ISO_LANGS.get(code)
  }

  def default: Lang = en

  // iso639 code to babel, note that this misses important languages such as
  // Chinese, Japanese, Korean and Arabic (for which extra pains are required)
  private val ISO_LANGS = Map(
    af.code -> af,
    id.code -> id,
    ms.code -> ms,
    bg.code -> bg,
    ca.code -> ca,
    hr.code -> hr,
    cs.code -> cs,
    da.code -> da,
    nl.code -> nl,
    en.code -> en,
    enUS.code -> enUS,
    enUK.code -> enUK,
    enCA.code -> enCA,
    enAU.code -> enAU,
    enNZ.code -> enNZ,
    et.code -> et,
    fi.code -> fi,
    fr.code -> fr,
    frFR.code -> frFR,
    frCA.code -> frCA,
    frCH.code -> frCH,
    gl.code -> gl,
    de.code -> de,
    deDE.code -> deDE,
    deAT.code -> deAT,
    deCH.code -> deCH,
    el.code -> el,
    he.code -> he,
    hu.code -> hu,
    is.code -> is,
    ga.code -> ga,
    it.code -> it,
    la.code -> la,
    no.code -> no,
    pl.code -> pl,
    pt.code -> pt,
    ptBR.code -> ptBR,
    ro.code -> ro,
    ru.code -> ru,
    es.code -> es,
    sk.code -> sk,
    sl.code -> sl,
    sv.code -> sv,
    sr.code -> sr,
    tr.code -> tr,
    uk.code -> uk)

}