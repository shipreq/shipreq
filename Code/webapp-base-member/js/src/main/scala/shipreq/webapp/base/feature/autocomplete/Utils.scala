package shipreq.webapp.base.feature.autocomplete

import japgolly.microlibs.utils.{Utils => Util}
import scala.collection.View
import shipreq.webapp.base.text.GrammarSpec
import shipreq.webapp.base.data.{Contextualise, Plain}
import shipreq.webapp.base.jsfacade.TextComplete.Strategy
import Implicits.autoLiftTextCompleteStrategy

object Utils {
  type Strategies = Vector[Strategy[_]]

  final class Context(val prefixRegex: String,
                      val suffixRegex: String,
                      val applyContext: String => String) {

    // Util.regexEscapeAndWrap turns empty strings into (?:) which is fine
    // val acSuffix = if (suffixRegex.isEmpty) "$" else suffixRegex + "?$"

    def apply[A](mainRegex     : String,
                 replacementA  : A => String,
                 replacementEnd: String,
                 rest          : Strategy.Step3b[A] => Strategy.Ready[A]): Contextualise => Strategies = {

      case Contextualise =>
        rest(Strategy.builder
          .regex(s"$prefixRegex$mainRegex$suffixRegex?$$", index = 1)
          .replace(s => applyContext(replacementA(s)) + replacementEnd))
          .result()

      case Plain =>
        rest(Strategy.builder
          .regex(s"(^|\\s)$prefixRegex?$mainRegex$suffixRegex?$$", index = 2)
          .replace(s => "$1" + replacementA(s) + replacementEnd))
          .result()
    }
  }

  object Context {
    def apply(s: GrammarSpec.Surrounds): Context = {
      val (a, b) = s.parsing.regexEscapeAndWrap
      new Context(a, b, s.display.apply)
    }

    def literal(pre: String, suf: String): Context =
      new Context(Util regexEscapeAndWrap pre, Util regexEscapeAndWrap suf, pre + _ + suf)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Query[A] = String => Iterable[A]

  /**
   * Prevents auto-complete when the search term is empty.
   * Prevents showing all options without criteria.
   *
   * Note that you can prevent this in your `match` regex.
   */
  def ignoreEmptyTerm[A](f: Query[A]): Query[A] =
    term =>
      if (term.isEmpty)
        Nil
      else
        f(term)

  /**
   * Prevents auto-complete when the only result just what the user already has typed.
   */
  def ignorePerfectMatch[A](query: Query[A])(perfectMatch: (String, A) => Boolean): Query[A] =
    term => {
      val r = query(term)
      if (r.sizeCompare(1) == 0 && perfectMatch(term, r.head))
        Nil
      else
        r
    }

  def ignorePerfectMatchStr(query: Query[String]): Query[String] =
    ignorePerfectMatch(query)(_ == _)

  /**
   * Normalises term and options before comparison.
   *
   * @param options Pre-sorted options.
   */
  def normalisedStringQuery[A](norm: String => String, cmp: (String, String) => Boolean, options: Iterable[String]): Query[String] = {
    val os = View.from(options.iterator.map(s => (norm(s), s)))
    term => {
      val t2 = norm(term)
      os.filter(o => cmp(o._1, t2)).map(_._2)
    }
  }

  /**
   * Matches options containing the search string, where case is ignored.
   *
   * @param options Pre-sorted options.
   */
  def caseInsensitiveContains(options: Iterable[String]): Query[String] =
    ignorePerfectMatchStr(
      normalisedStringQuery(_.toLowerCase, _ contains _, options))

  /**
   * Matches options containing the search string, where case is ignored.
   *
   * @param options Pre-sorted options.
   */
  def caseInsensitiveStartsWith(options: Iterable[String]): Query[String] =
    ignorePerfectMatchStr(
      normalisedStringQuery(_.toLowerCase, _ startsWith _, options))
}
