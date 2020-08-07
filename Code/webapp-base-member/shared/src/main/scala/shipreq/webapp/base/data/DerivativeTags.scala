package shipreq.webapp.base.data

import shipreq.base.util.{Disabled, Enabled}

final case class DerivativeTags(enabled: Enabled, rules: DerivativeTags.Rules) {

  def tagIdIterator(): Iterator[ApplicableTagId] =
    rules.iterator.flatMap(x => x._1.lo :: x._1.hi :: x._2 :: Nil)
}

object DerivativeTags {

  val emptyDisabled: DerivativeTags =
    apply(Disabled, UnivEq.emptyMap)

  final case class TagPair(lo: ApplicableTagId, hi: ApplicableTagId) {
    assert(lo.value < hi.value)
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
