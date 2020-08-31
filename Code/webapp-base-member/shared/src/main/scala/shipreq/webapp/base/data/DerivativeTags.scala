package shipreq.webapp.base.data

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

  def add(tags  : Set[ApplicableTagId],
          newTag: ApplicableTagId,
          filter: ApplicableTagId => Boolean = _ => true): Set[ApplicableTagId] = {

    var result: Set[ApplicableTagId] = null

    val it = tags.iterator
    while ((result eq null) && it.hasNext) {
      val t = it.next()
      if (t !=* newTag) {
        val p = TagPair(t, newTag)
        rules.get(p) match {
          case Some(r) =>
            if (filter(r))
              result = reduce(tags - t + r, filter)
          case None =>
        }
      }
    }

    if (result ne null)
      result
    else
      tags + newTag
  }

  def reduce(tags       : Set[ApplicableTagId],
             filter     : ApplicableTagId => Boolean = _ => true,
             recursively: Boolean = true): Set[ApplicableTagId] = {

    val it1 = tags.iterator
    while (it1.hasNext) {
      val t1 = it1.next()

      val it2 = tags.iterator
      while (it2.hasNext) {
        val t2 = it2.next()

        if (t1.value < t2.value) {
          val p = TagPair(t1, t2)
          rules.get(p) match {
            case Some(r) =>
              if (filter(r)) {
                val next = tags - t1 - t2 + r
                return if (recursively) reduce(next) else next
              }
            case None    =>
          }
        }
      } // it2
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
