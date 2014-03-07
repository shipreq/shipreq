package shipreq.webapp.feature.validation

import java.util.regex.Pattern
import scala.util.matching.Regex

trait TextReplacement {
  def apply(in: String): String
}

case class RegexReplacement(regex: Regex, replacement: String) extends TextReplacement {
  override def apply(in: String) = regex.replaceAllIn(in, replacement)
}

case class CharReplacement(from: Char, replacement: Char) extends TextReplacement {
  override def apply(in: String) = in.replace(from, replacement)
}

object Trim extends TextReplacement {
  override def apply(in: String) = in.trim
}

object TextReplacements {

  private [this] val PunctuationOrSymbol = """[\p{S}\p{P}]"""
  private def symbolReplacement(from: String, to: String): TextReplacement = {
    val f = Pattern.quote(from)
    RegexReplacement(s"(?<!$PunctuationOrSymbol)$f(?!$PunctuationOrSymbol)".r, to)
  }

  @inline final def perform(replacements: List[TextReplacement])(z: String): String =
    (z /: replacements)((t, r) => r(t))

  val General: List[TextReplacement] = List(
    symbolReplacement("<=", "≤"),
    symbolReplacement(">=", "≥")
  )

  val Whitespace: List[TextReplacement] = List(
    RegexReplacement("\r\n?".r, "\n"),
    CharReplacement('\t', ' '),
    Trim
  )

  val GeneralWithWhitespace = General ++ Whitespace
}
