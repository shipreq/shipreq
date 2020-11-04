package shipreq.webapp.member.feature.autocomplete.strategies

import japgolly.microlibs.utils.{Utils => Util}
import shipreq.webapp.member.data.{Contextualise, Plain}
import shipreq.webapp.member.jsfacade.TextComplete.Strategy
import shipreq.webapp.member.text.{Grammar, GrammarSpec}

private[strategies] final class Context(val prefixRegex : String,
                                        val suffixRegex : String,
                                        val applyContext: String => String,
                                        val prefixGroups: Int) {

  // We want to only make suggestions when the suffix isn't present.
  // If it is present, then the user has already completed it.
  // Eg. If I type `[mf9`, please suggest; but if I type `[mf9]` don't suggest anything -- I've just indicated that
  // I've made my decision.
  private val notSuffix =
    suffixRegex match {
      case "" | "(?:)" => ""
      case suffix      => s"(?!$suffix)"
    }

  private val prefixCapture1 =
    1.to(prefixGroups).iterator.map("$" + _.toString).mkString

  private val prefixCapture2 =
    prefixCapture1 + "$" + (prefixGroups + 1).toString

  def apply[A](mainRegex     : String,
               replacementA  : A => String,
               replacementEnd: String,
               rest          : Strategy.Step3b[A] => Strategy.Ready[A]): Contextualise => Strategies = {

    case Contextualise =>
      rest(Strategy.builder
        .regex(s"$prefixRegex$mainRegex$notSuffix$$", index = 1 + prefixGroups)
        .replace(s => prefixCapture1 + applyContext(replacementA(s)) + replacementEnd))
        .result()

    case Plain =>
      rest(Strategy.builder
        .regex(s"(^|\\s)$prefixRegex?$mainRegex$notSuffix$$", index = 2 + prefixGroups)
        .replace(s => prefixCapture2 + replacementA(s) + replacementEnd))
        .result()
  }
}

private[strategies] object Context {

  def apply(s: GrammarSpec.Surrounds): Context = {
    val (a, b) = s.parsing.regexEscapeAndWrap
    new Context(a, b, s.display.apply, 0)
  }

  def literal(pre: String, suf: String): Context =
    new Context(
      prefixRegex  = Util regexEscapeAndWrap pre,
      suffixRegex  = Util regexEscapeAndWrap suf,
      applyContext = pre + _ + suf,
      prefixGroups = 0)

  val reflink: Context =
    Context(Grammar.reflinkSurround)

  val hashtag: Context = {
    assert(Grammar.hashRefKey.prefix == "#")
    new Context(
      prefixRegex  = "(^|[^#])#",
      suffixRegex  = "",
      applyContext = "#" + _,
      prefixGroups = 1,
    )
  }

}
