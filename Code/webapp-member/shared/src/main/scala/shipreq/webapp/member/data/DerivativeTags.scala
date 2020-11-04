package shipreq.webapp.member.data

import shipreq.base.util.{Disabled, Enabled}

final case class DerivativeTags(enabled: Enabled, rules: DerivativeTags.Rules) {
  import DerivativeTags.TagPair

  def filterRulesByResult(f: ApplicableTagId => Boolean): DerivativeTags =
    copy(rules = rules.filter(x => f(x._2)))

  def withoutRuleFor(x: ApplicableTagId, y: ApplicableTagId): DerivativeTags =
    if (x ==* y)
      this
    else {
      val k = TagPair(x, y)
      copy(rules = rules - k)
    }

  def tagIdIterator(): Iterator[ApplicableTagId] =
    rules.iterator.flatMap(x => x._1.lo :: x._1.hi :: x._2 :: Nil)

  def combineOption(tag1: ApplicableTagId, tag2: ApplicableTagId): Option[ApplicableTagId] =
    if (tag1 ==* tag2)
      tag1.some
    else {
      val pair = TagPair(tag1, tag2)
      rules.get(pair)
    }

  def derive(tags       : Set[ApplicableTagId],
             tagOrder   : Ordering[ApplicableTagId],
             srcFilter  : ApplicableTagId => Boolean,
             tgtFilter  : ApplicableTagId => Boolean,
             recursively: Boolean                    = true,
            ): Set[ApplicableTagId] = {

    // Tags must be sorted
    // https://shipreq.com/project/d6My#/reqs/SC-7
    val orderedTags = tags.toArray
    scala.util.Sorting.quickSort(orderedTags)(tagOrder)

    var i1, i2 = 0
    while (i1 < orderedTags.length) {
      val t1 = orderedTags(i1)

      if (srcFilter(t1)) {
        i2 = 0
        while (i2 < orderedTags.length) {
          if (i1 != i2) {
            val t2 = orderedTags(i2)

            if (srcFilter(t2)) {
              val p = TagPair(t1, t2)
              rules.get(p) match {
                case Some(r) =>
                  if (tgtFilter(r)) {
                    val next = tags - t1 - t2 + r
                    return if (recursively) derive(next, tagOrder, srcFilter, tgtFilter) else next
                  }
                case None =>
              }
            }

          }
          i2 += 1
        } // it2
      }

      i1 += 1
    } // it1

    tags
  }
}

object DerivativeTags {

  val emptyDisabled: DerivativeTags =
    apply(Disabled, UnivEq.emptyMap)

  final case class TagPair(lo: ApplicableTagId, hi: ApplicableTagId) {
    assert(lo.value < hi.value)

    def forAll(f: ApplicableTagId => Boolean): Boolean =
      f(lo) && f(hi)
  }

  object TagPair {
    def apply(a: ApplicableTagId, b: ApplicableTagId): TagPair =
      if (a.value < b.value)
        new TagPair(a, b)
      else
        new TagPair(b, a)

    implicit def univEq: UnivEq[TagPair] = UnivEq.derive
  }

  type Rules = Map[TagPair, ApplicableTagId]

  implicit def univEq: UnivEq[DerivativeTags] = UnivEq.derive
}
