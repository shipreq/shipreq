package shipreq.webapp.base.feature.autocomplete.strategies

import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.jsfacade.TextComplete.Strategy
import shipreq.webapp.base.text.{Atom, PlainText, Text, TextSearch}

object ProjectStrategies {
  import Atom.TypeGroup

  type ReqItem = shipreq.webapp.base.feature.autocomplete.strategies.ReqItem
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

  def req(reqItems: ReqItems, textSearch: TextSearch): Contextualise => Strategies = {
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
