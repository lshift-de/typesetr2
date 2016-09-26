package net.lshift.typesetr
package pandoc

import org.python.util.PythonInterpreter
import scala.util.matching.Regex

private class PygmentsFormatter {

  def format(from: String, kind: PygmentsFormatterKind)(implicit log: util.Logger): String = {
    val codeVar = "code"
    val resultVar = "result"
    val decodedText = preformattedText(from)
    log.info("Decoded pre-formatted text: " + decodedText)

    val interpreter = new PythonInterpreter()
    interpreter.set(codeVar, decodedText)
    interpreter.exec(pythonCode(codeVar, resultVar, guessLang(decodedText), kind))
    val result = interpreter.get(resultVar, classOf[String])
    interpreter.cleanup()

    result
  }

  // Pandoc introduces some weird formatting, since it does not know
  // that the text is supposed to be left as-is.
  // So we have to fix it.
  def preformattedText(body: String): String =
    VerbatimDecoding.foldLeft(body) {
      case (b, (from, to)) =>
        b.replaceAllLiterally(from, to)
    }

  // TODO: This is probably far from the complete list
  //       of characters encoded by Pandoc.
  //       Will need to be adapted on a per-need basis.
  val VerbatimDecoding = Map(
    "\\_" -> "_",
    "\\%" -> "%",
    "\\\\" -> "",
    Writer.TypesetrPreSpace -> " ",
    "{[}" -> "[",
    "{]}" -> "]")

  def pythonCode(codeVar: String, resultVar: String, langName: String, formatter: PygmentsFormatterKind) = {
    s"""|from pygments import highlight
        |from pygments.lexers import get_lexer_by_name
        |from pygments.formatters import ${formatter}
        |
        |$resultVar = highlight($codeVar, get_lexer_by_name(\"$langName\"), ${formatter}())
     """.stripMargin
  }

  // FIXME(alexander): this is a mere toy, obviously, but
  // good enough for testing purposes. It shouldn't be too hard
  // to make this work well with a probabilistic approach.
  def guessLang(s: String): String =
    s match {
      case python(name)     => name
      case ruby(name)       => name
      case java(name)       => name
      case c(name)          => name
      case javascript(name) => name
      case css(name)        => name
      case html(name)       => name
      case latex(name)      => name
      case clojure(name)    => name
      case sql(name)        => name
      case xml(name)        => name
      case _                => "bash"
    }

  sealed abstract class P {
    val reg: String
    def unapply(s: String): Option[String] = {
      val r = new Regex(reg)
      if (r.findFirstIn(s).nonEmpty) Some(this.getClass().getSimpleName.stripSuffix("$")) else None
    }
  }

  object python extends P {
    val reg = "(def|class) \\w+\\(.*\\):\\s|lambda [^()]+:|if \\w.*"
  }

  object ruby extends P {
    val reg = "\\send\n|(do\\s+|\\{)\\|\\w "
  }

  object java extends P {
    val reg = "^(public|private|protected|\\s+) class \\w+(implements|extends|\\{)"
  }

  object c extends P {
    val reg = "^#(define|include|ifdef)\\b|\\s\\*+[a-z]"
  }

  object javascript extends P {
    val reg = "function [\\w \\$]*\\([^()]+\\)\\{|(if|for|switch|while|catch) \\(.*\\)\\s+\\{"
  }

  object css extends P {
    val reg = "\\{\n?\\s*[\\w-]+:"
  }

  object html extends P {
    val reg = "<(div|span|h[1-6]|a|b|i|p|li|dt|tr|br|hr|img|script|meta|style|input)\b(\\s+\\w|>)"
  }

  object latex extends P {
    val reg = "\\@?[A-Za-z]{2,}[\\{\\[]"
  }

  object clojure extends P {
    val reg = "\\(defn?\b"
  }

  object sql extends P {
    val reg = "^(SELECT|INSERT INTO|DELETE FROM|UPDATE|CREATE|DROP)\b"
  }

  object xml extends P {
    val reg = "<?|</|/>"
  }

}

object PygmentsFormatter {
  def apply(from: String, kind: PygmentsFormatterKind)(implicit log: util.Logger): String =
    new PygmentsFormatter().format(from, kind)
}
