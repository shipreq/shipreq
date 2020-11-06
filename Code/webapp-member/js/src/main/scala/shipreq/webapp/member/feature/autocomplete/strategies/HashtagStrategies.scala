package shipreq.webapp.member.feature.autocomplete.strategies

import japgolly.microlibs.stdlib_ext.MutableArray
import shipreq.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.derivation._
import shipreq.webapp.member.project.text.Grammar

/** #ISSUE #TAG */
private[strategies] object HashtagStrategies {

  def apply(candidates: IterableOnce[HashRefKey]): Contextualise => Strategies = {
    import Grammar.{hashRefKey => G}
    // ↓ `- 2` cos 1 is already firstChar, and another 1 is unnecessary in that if the tag is complete, there's nothing to suggest
    val mainRegex = s"(|${G.firstChar.one}${G.midChars.one}{0,${G.length.total.max - 2}})"
    val terms     = MutableArray(candidates.iterator.map(_.value)).sort.arraySeq
    val searchFn  = Query.caseInsensitiveContains(terms)
    Context.hashtag[String](mainRegex, Identity.apply, "", _.search(searchFn))
  }

  def forIssuesAndTags(issues    : IterableOnce[CustomIssueType],
                       tags      : IterableOnce[ApplicableTag],
                       filterDead: FilterDead): Contextualise => Strategies =
    apply(
      filterDead(issues.iterator)(_.live).map(_.key) ++
      filterDead(tags  .iterator)(_.live).map(_.key))

  def forTags(legal: Iterable[ApplicableTag], filterDead: FilterDead): Contextualise => Strategies =
    forIssuesAndTags(Nil, legal, filterDead)

  def forProject(project   : Project,
                 filterDead: FilterDead,
                 issues    : Boolean,
                 tags      : Boolean,
                 naTags    : NaTags): Contextualise => Strategies =
    if (issues || tags) {

      val issueOptions: Iterable[CustomIssueType] =
        if (issues)
          project.config.customIssueTypes.values
        else
          Nil

      val tagOptions: IterableOnce[ApplicableTag] =
        if (tags)
          project.config.tags.applicableTagIterator().filter(_.live is Live).filter(t => !naTags.set.contains(t.id))
        else
          Nil

      forIssuesAndTags(issueOptions, tagOptions, filterDead)
    } else
      _ => Vector.empty

}
