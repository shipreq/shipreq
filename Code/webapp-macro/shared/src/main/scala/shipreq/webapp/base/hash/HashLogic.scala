package shipreq.webapp.base.hash

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import EvoHashModule._

object HashLogic {

  // Optimisations:
  // - Merge hash recs
  // - Make existing code efficient
  // - Cache prev hashScopes
  // - Make prev hashScopes lazy

  // - Make HashScope.To lazy (and specialised)

  //  def consolidate[A, B](as: Vector[A])(hashRecs: A => HashRec.Collection, ab: A => B): List[(NonEmptyVector[B], HashRec.Collection)] = {
  // TODO BM list ::= vs Vector.slice?
  //  def consolidate[A, B](as: Vector[A])(hashRecs: A => HashRec.Collection, ab: A => B): List[(List[B], HashRec.Collection)] = {


  // TOOD BatchHashes is actually isomorphic to (old) HashRec.Collection....
  // TODO Can it be built fast without fuss? Dep on HashScope.To structure
  // Inverse:
  // - changes :: Project -> Project -> HashRec.Collection


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

    private val propogation: Scheme => HashRecs => ScopeMap[Option[Int]] =
      if (schemeRegistry.schemes.length ==* 1)
        _ => _emptyScopeMap
      else {

        val om =
          for {
            s1 <- schemeRegistry.schemes.whole
            s2 <- schemeRegistry.schemes.whole if s2 !=* s1
            hashFns = s2.hashFns.filter { case (scope, h) => s1.hashFns.get(scope).exists(_.ver <= h.ver) }
          } yield s1 -> (s2 -> hashFns)

        val omg: Map[Scheme, Vector[(Scheme, ScopeMap[VersionedHashFn])]] =
          om.groupBy(_._1).mapValuesNow(_.map(_._2).filter(_._2.nonEmpty))

        //            for ((a, b) <- omg)
        //              println(s"$a -- ${b.map(_.map2(_.map(_.version)))}")


        scheme => {
          omg.get(scheme) match {
            case None =>
              _emptyScopeMap

            case Some(xx) =>
              hrs =>
                xx.iterator
                  .map { case (scheme2, hashFns) => hrs.get(scheme2).map(_ -> hashFns) }
                  .filterDefined
                  .flatMap { case (hr2, hf) => hf.keysIterator.map(s => hr2.get(s).map((s, _))).filterDefined }
                  .toMap
          }
        }
      }

    val oneByOne: Vector[A] => Batches =
      _.map(a => Batch(ab(a) :: Nil, hashRecs(a)))(collection.breakOut)

    val optimal: Vector[A] => Batches = as => {
      var results: Batches = Nil
      var i = as.length
      while (i > 0) {
        i -= 1
        val a = as(i)
        val b = ab(a)
        val curSRs = hashRecs(a)

        results = results match {

          case Nil =>
            val hrs = if (curSRs.values.isEmpty)
              forcePass
            else
              curSRs
            Batch(b :: Nil, hrs) :: Nil

          case Batch(nextBs, nextSRs) :: nextResults =>

            if (nextSRs eq forcePass) {
              if (curSRs.values.isEmpty)
                Batch(b :: nextBs, forcePass) :: nextResults
              else
                Batch(b :: Nil, curSRs) :: results

            } else if (curSRs.keySet ==* nextSRs.keySet) {
              val xx =
                curSRs.map { case (scheme, curRs) =>
                  val nextRs = nextSRs(scheme)
                  var nextHashes = nextRs
                  curRs.foreach { case (s, h) =>
                    if (!nextRs.contains(s))
                      nextHashes = nextHashes.updated(s, h)
                  }
                  scheme -> nextHashes
                }
              Batch(b :: nextBs, xx) :: nextResults

            } else if (curSRs.values.isEmpty)
              Batch(b :: Nil, forcePass) :: results

            else {
//              val isThereSchemeOverlap = curSRs.keysIterator.exists(nextSRs.contains)
//              if (isThereSchemeOverlap) {
                Batch(b :: Nil, curSRs) :: results
//              } else {
//                val nextSRs2 =
//                  nextSRs.map { case (scheme, byScheme) =>
//                    var hashes2 = byScheme
//                    propogation(scheme)(curSRs).foreach { case (scope, hash) =>
//                      if (!hashes2.contains(scope))
//                        hashes2 = hashes2.updated(scope, hash)
//                    }
//                    scheme -> hashes2
//                  }
//                Batch(b :: Nil, curSRs) :: Batch(nextBs, nextSRs2) :: nextResults
//                Batch(b :: Nil, curSRs) :: results
//              }
            }
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
      err               ← HashDiscrepancy.cmp(scheme, scope, actual = actual, expect = expect)
    } errs ::= err
    errs
  }
}

/*
SHARED
======
BinCodecEvents  - binary codec
ProjectTemplate - changes :: Project -> Project -> HashRec.Collection
VerifiedEvent   - case class VerifiedEvent(event: Event, hashRecs: HashRec.Collection)
ApplyEvent      - verification

SERVER
======
EventDao      - json codec
DbInterpreter - DB -> Model
DB            - DB interface
ApplyNewEvent - changes :: Project -> Project -> HashRec.Collection
 */