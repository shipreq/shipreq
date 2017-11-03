package shipreq.webapp.base.hash2

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
              val isThereSchemeOverlap = curSRs.keysIterator.exists(nextSRs.contains)
              if (isThereSchemeOverlap) {
                Batch(b :: Nil, curSRs) :: results
              } else {
                val nextSRs2 =
                  nextSRs.map { case (scheme, byScheme) =>
                    var hashes2 = byScheme
                    propogation(scheme)(curSRs).foreach { case (scope, hash) =>
                      if (!hashes2.contains(scope))
                        hashes2 = hashes2.updated(scope, hash)
                    }
                    scheme -> hashes2
                  }
                Batch(b :: Nil, curSRs) :: Batch(nextBs, nextSRs2) :: nextResults
              }
            }
        }
      }

      results
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

//  /**
//    * Rules:
//    * - All arguments must correspond to the same HashScheme
//    * - prev & actual must contain values for all HashScopes in the HashScheme
//    */
//  def validateSingleScheme(hashScheme: HashScheme,
//                           prev      : HashScope.To[Int],
//                           actual    : HashScope.To[Int],
//                           expect    : HashScope.To[Option[Int]]): List[HashDiscrepancy] = {
//
//    assert(
//      expect.scopeIterator.forall(actual.contains),
//      s"HashScheme previously provided the hash for a scope, which seems to have been removed. THIS IS AGAINST ITS LAWS.")
//
//    var errors = List.empty[HashDiscrepancy]
//
//    def cmp(hashScope: HashScope, actual: Int, expect: Int) =
//      if (expect !=* actual)
//        errors ::= HashDiscrepancy(hashScheme, hashScope, expect = expect, actual = actual)
//
//    actual.foreach {x =>
//      val s = x._1
//      val ha = x._2
//      expect.get(s) match {
//        case Some(Some(he)) => cmp(s, actual = ha, expect = he)
//        case None           => cmp(s, actual = ha, expect = prev.need(s))
//        case Some(None)     => () // expectation of None means force pass - TODO SHOULD CARRY OVER UNTIL OVERRIDDEN?
//      }
//    }
//
//    errors
//  }
//
//  def validateMultiScheme(prevProject: Project, p: Project, recs: HashRec.Collection): List[HashDiscrepancy] = {
//    val byScheme = recs.groupBy(_.scheme)
//    var errors = List.empty[HashDiscrepancy]
//    for ((scheme, recs) <- byScheme) {
//      val prev  : HashScope.To[Int] = scheme.hash(prevProject)
//      val actual: HashScope.To[Int] = scheme.hash(p)
//      val expect: HashScope.To[Option[Int]] = HashScope.To(recs.map(r => r.scope -> r.hash).toMap)
//      val es = validateSingleScheme(scheme, prev, actual, expect)
//      if (es.nonEmpty)
//        errors :::= es
//    }
//    errors
//  }

//  def validateBatchHashes(p: Project, batchHashes: BatchHashes): List[HashDiscrepancy] = {
//    var errs = List.empty[HashDiscrepancy]
//    for {
//      (scheme, hashFns) <- batchHashes
//      (scope, hashOption) <- hashFns
//      hash <- hashOption
//      actual = scheme.hashFns.need(scope).hashFn(p)
//      err <- HashDiscrepancy.cmp(scheme, scope, actual = actual, expect = hash)
//    } errs ::= err
//      errs
//  }
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