package shipreq.webapp.member.feature.autocomplete.strategies

import shipreq.webapp.member.jsfacade.TextComplete.Strategy
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.Grammar

private[strategies] object SmallStrategies {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // <tex>

  lazy val tex: Strategies = {
    val tags = List(Grammar.texTag)
    Strategy.builder
      .regex("""(^|\s)<([a-z]+)$""", index = 2)
      .search(term => tags.filter(_ startsWith term))
      .replace2(tag => (s"$$1<$tag>", s"</$tag>"))
      .result()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Req type mnemonics

  def reqTypeMnemonics(reqTypes: ReqTypes, exclude: Set[String]): Strategies = {
    import Grammar.{reqTypeMnemonic => G}
    Strategy.builder
      .regex(s"(^|\\s|,)(|${G.caseInsensitiveRegexStr})$$", index = 2)
      .search(term =>
        reqTypes.liveSortedByMnemonic
          .iterator
          .filterNot(rt => exclude.contains(rt.mnemonic.value))
          .filter(_.mnemonic.value startsWith term)
          .map(r => s"${r.mnemonic.value}: ${r.name}")
      )
      .replace(rt => s"$$1${rt.takeWhile(_ != ':')}")
      .result()
  }

}
