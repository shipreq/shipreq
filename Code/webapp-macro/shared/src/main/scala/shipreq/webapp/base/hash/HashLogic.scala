package shipreq.webapp.base.hash

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import EvoHashModule._

object HashLogic {

  final case class Batch[Scope, Data, +A](elements: List[A], recs: HashRecs[Scope, Data])

  type Batches[Scope, Data, +A] = List[Batch[Scope, Data, A]]

  final case class Batcher[Scope: UnivEq, Data, A, B](ab: A => B,
                                                      hashRecs: A => EvoHashModule.HashRecs[Scope, Data],
                                                      schemeRegistry: EvoHashModule.Schemes[Scope, Data]) {

    type VersionedHashFn = EvoHashModule.VersionedHashFn[Data]
    type Scheme          = EvoHashModule.Scheme[Scope, Data]
    type Schemes         = EvoHashModule.Schemes[Scope, Data]
    type ScopeMap[+X]    = EvoHashModule.ScopeMap[Scope, X]
    type HashRecs        = EvoHashModule.HashRecs[Scope, Data]
    type Batch           = HashLogic.Batch[Scope, Data, B]
    type Batches         = HashLogic.Batches[Scope, Data, B]

    private val forcePass: HashRecs =
      schemeRegistry
        .schemes
        .iterator
        .map(s => s -> s.hashFns.mapValuesNow(_ => Option.empty[Int]))
        .toMap

    private val emptyScopeMap: ScopeMap[Nothing] =
      UnivEq.emptyMap[Scope, Nothing]

    private val _emptyScopeMap: Any => ScopeMap[Nothing] =
      _ => emptyScopeMap

    val oneByOne: Traversable[A] => Batches =
      _.map(a => Batch(ab(a) :: Nil, hashRecs(a)))(collection.breakOut)

    val optimal: IndexedSeq[A] => Batches = as => {
      var results: Batches = Nil
      var i = as.length
      while (i > 0) {
        i -= 1
        val a = as(i)
        val b = ab(a)
        val curSRs = hashRecs(a)

        results = results match {
          case Batch(nextBs, nextSRs) :: nextResults =>

            if (nextSRs eq forcePass) {
              if (curSRs.values.isEmpty)
                Batch(b :: nextBs, forcePass) :: nextResults
              else
                Batch(b :: Nil, curSRs) :: results
            }

            else if (curSRs.keySet ==* nextSRs.keySet) {
              val hrs: HashRecs =
                curSRs.map { case (scheme, curRs) =>
                  val nextRs = nextSRs(scheme)
                  var nextHashes = nextRs
                  curRs.foreach { case (s, h) =>
                    if (!nextRs.contains(s))
                      nextHashes = nextHashes.updated(s, h)
                  }
                  scheme -> nextHashes
                }
              Batch(b :: nextBs, hrs) :: nextResults
            }

            else if (curSRs.values.isEmpty)
              Batch(b :: Nil, forcePass) :: results

            else
              Batch(b :: Nil, curSRs) :: results

          case Nil =>
            val hrs: HashRecs =
              if (curSRs.values.isEmpty)
                forcePass
              else
                curSRs
            Batch(b :: Nil, hrs) :: Nil
        }
      }

      results
    }
  }

  def validate[Scope, Data](recs   : EvoHashModule.HashRecs[Scope, Data],
                            before : Data,
                            current: Data): List[HashDiscrepancy[Scope, Data]] = {
    var errs = List.empty[HashDiscrepancy[Scope, Data]]
    for {
      (scheme, expects) ← recs
      (scope, hashFn)   ← scheme.hashFns
      expect            ← expects.getOrElse(scope, Some(hashFn(before)))
      actual            = hashFn(current)
    } if (actual !=* expect)
      errs ::= HashDiscrepancy(scheme, scope, actual = actual, expect = expect)
    errs
  }
}
