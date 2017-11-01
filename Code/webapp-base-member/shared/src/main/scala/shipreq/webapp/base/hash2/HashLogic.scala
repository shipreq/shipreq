package shipreq.webapp.base.hash2

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import shipreq.webapp.base.data.Project

final case class HashDiscrepancy(scheme: HashScheme,
                                 scope : HashScope,
                                 expect: Int,
                                 actual: Int) {
  assert(actual != expect)

  def scopeVer: HashScope.Version =
    scheme.hashFns.need(scope).version

  def msg = s"$scheme.$scope(v$scopeVer) $actual should be $expect."
}

object HashDiscrepancy {
//  def cmp(hashScheme: HashScheme,
//          hashScope : HashScope,
//          actual    : Int,
//          expect    : Int): Option[HashDiscrepancy] =
//    Option.when(expect !=* actual)(
//      HashDiscrepancy(hashScheme, hashScope, expect = expect, actual = actual))
}

object HashLogic {

  /**
    * Rules:
    * - All arguments must correspond to the same HashScheme
    * - prev & actual must contain values for all HashScopes in the HashScheme
    */
  def validateSingleScheme(hashScheme: HashScheme,
                           prev      : HashScope.To[Int],
                           actual    : HashScope.To[Int],
                           expect    : HashScope.To[Option[Int]]): List[HashDiscrepancy] = {

    assert(
      expect.scopeIterator.forall(actual.contains),
      s"HashScheme previously provided the hash for a scope, which seems to have been removed. THIS IS AGAINST ITS LAWS.")

    var errors = List.empty[HashDiscrepancy]

    def cmp(hashScope: HashScope, actual: Int, expect: Int) =
      if (expect !=* actual)
        errors ::= HashDiscrepancy(hashScheme, hashScope, expect = expect, actual = actual)

    actual.foreach {x =>
      val s = x._1
      val ha = x._2
      expect.get(s) match {
        case Some(Some(he)) => cmp(s, actual = ha, expect = he)
        case None           => cmp(s, actual = ha, expect = prev.need(s))
        case Some(None)     => () // expectation of None means force pass - TODO SHOULD CARRY OVER UNTIL OVERRIDDEN?
      }
    }

    errors
  }

  def validateMultiScheme(prevProject: Project, p: Project, recs: HashRec.Collection): List[HashDiscrepancy] = {
    val byScheme = recs.groupBy(_.scheme)
    var errors = List.empty[HashDiscrepancy]
    for ((scheme, recs) <- byScheme) {
      val prev  : HashScope.To[Int] = scheme.hash(prevProject)
      val actual: HashScope.To[Int] = scheme.hash(p)
      val expect: HashScope.To[Option[Int]] = HashScope.To(recs.map(r => r.scope -> r.hash).toMap)
      val es = validateSingleScheme(scheme, prev, actual, expect)
      if (es.nonEmpty)
        errors :::= es
    }
    errors
  }

  // Optimisations:
  // - Merge hash recs
  // - Make existing code efficient
  // - Cache prev hashScopes
  // - Make prev hashScopes lazy

  // - Make HashScope.To lazy (and specialised)

//  def consolidate[A, B](as: Vector[A])(hashRecs: A => HashRec.Collection, ab: A => B): List[(NonEmptyVector[B], HashRec.Collection)] = {
  // TODO BM list ::= vs Vector.slice?
  def consolidate[A, B](as: Vector[A])(hashRecs: A => HashRec.Collection, ab: A => B): List[(List[B], HashRec.Collection)] = {
    var results = List.empty[(List[B], HashRec.Collection)]
    var bs = List.empty[B]
    var i = as.length
    while (i > 0) {
      i -= 1
      val a = as(i)
      val b = ab(a)
      // compare hashschemes to next
    }
    results
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