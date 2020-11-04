package shipreq.webapp.member.feature.autocomplete.strategies

import shipreq.webapp.member.data._
import shipreq.webapp.member.data.derivation._
import shipreq.webapp.member.jsfacade.TextComplete.Strategy
import shipreq.webapp.member.text.{Atom, PlainText, Text, TextSearch}

object ProjectStrategies {
  import Atom.TypeGroup

  type ReqItem = shipreq.webapp.member.feature.autocomplete.strategies.ReqItem
  val  ReqItem = shipreq.webapp.member.feature.autocomplete.strategies.ReqItem

  type ReqItems = ArraySeq[ReqItem]

  def hashtag(project   : Project,
              filterDead: FilterDead,
              issues    : Boolean,
              tags      : Boolean,
              naTags    : NaTags): Contextualise => Strategies =
    HashtagStrategies.forProject(
      project    = project,
      filterDead = filterDead,
      issues     = issues,
      tags       = tags,
      naTags     = naTags,
    )

  def req(reqItems: ReqItems, textSearch: TextSearch)(implicit s: ReqItem.Style): Contextualise => Strategies = {
    val candidates = RefStrategies.combineCandidates(
      RefStrategies.candidatesByPubid(reqItems),
      RefStrategies.candidatesByTitle(reqItems, textSearch),
    )
    RefStrategies(candidates)
  }

  def reqCodePrefixes(trie: ReqCode.Trie): Strategies =
    ReqCodeStrategies.prefixes(trie)

  def reqItems(p: Project, pt: PlainText.ForProject.AnyCtx): ReqItems =
    RefStrategies.reqItems(p, pt, p.content.reqs.all)

  def reqTypeMnemonics(reqTypes: ReqTypes, exclude: Set[String]): Strategies =
    SmallStrategies.reqTypeMnemonics(reqTypes, exclude)

  def richText(text      : Text.Generic,
               project   : Project,
               naTags    : NaTags,
               plainText : PlainText.ForProject.AnyCtx,
               textSearch: TextSearch): Strategies = {
    implicit def style = ReqItem.Style.IdAndTitle
    val s = Vector.newBuilder[Strategy[_]]

    s ++= HashtagStrategies.forProject(
      project    = project,
      filterDead = HideDead,
      issues     = text.supports(TypeGroup.Issue),
      tags       = text.supports(TypeGroup.TagRef),
      naTags     = naTags)(
      Contextualise)

    if (text.supports(TypeGroup.ContentRef)) {
      val reqItems = this.reqItems(project, plainText)
      val candidates = RefStrategies.combineCandidates(
        RefStrategies.candidatesByPubid(reqItems),
        ReqCodeStrategies.refCandidates(project, plainText),
        RefStrategies.candidatesByTitle(reqItems, textSearch),
      )
      s ++= RefStrategies(candidates)(Contextualise)
    }

    if (text.supports(Atom.Type.TeX))
      s ++= SmallStrategies.tex

    s.result()
  }

  @inline def tag(legal: Iterable[ApplicableTag], filterDead: FilterDead) =
    HashtagStrategies.forTags(legal, filterDead)

}
