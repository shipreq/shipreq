package shipreq.webapp.base.hash2

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import nyaya.gen._
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import scalaz.syntax.applicative._
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen._
import HashLogic.{Batcher, Batches}
import HashSchemes.EvolutionOp
import shipreq.webapp.base.data.Project

object Hash2Tests extends TestSuite {

  type HI = (HashRecs, Int)

  class BatchingTest(batcher: Batcher[HI, Int],
                     batch: Vector[HI] => Batches[Int],
                     hs: Vector[HashRecs]) {
    private val E = EvalOver(hs)

    private val his = hs.zipWithIndex
    private val batches = batch(his)

    private def elements =
      E.equal("consolidated batched elements = unbatched elements",
        batches.flatMap(_._1).toVector,
        his.map(_._2))

    private def latestHashRecIntact = {
      var p = E.pass
      for {
        expects       ← hs.lastOption
        actuals       = batches.last._2
        byScheme      ← expects.values
        scheme        = byScheme.scheme
        subActual     = actuals(scheme)
        tmp           ← byScheme.hashes.temp
        (scope, hash) = tmp
        actualValue   = subActual.flatMap(_.hashes get scope)
        expectValue   = Some(hash)
      } if (actualValue ≠ expectValue)
        p &= E.fail("The latest input HashRec, should be intact in the last batch",
          s"$scheme $scope: $actualValue ≠ $expectValue}")
      p
    }

    private def allScopesPerScheme =
      E.forall(batches.flatMap(_._2.values))(byScheme =>
        E.equal(s"${byScheme.scheme} scopes", byScheme.hashes.scopeSet, byScheme.scheme.hashFns.scopeSet))
        .rename("All scopes exist per scheme")

    lazy val all =
      elements & latestHashRecIntact // TODO & allScopesPerScheme
  }

  /*
  // TODO associativity
  def consPropAss[A: Equal](f: Vector[(A, HashRecs)] => Batches[A],
                            a: Vector[(A, HashRecs)],
                            b: Vector[(A, HashRecs)],
                            c: Vector[(A, HashRecs)]): Unit = {
    implicit def autoUnbatch(b: Batches[A]): Vector[(A, HashRecs)] = b.flatMap(as => )
    f(a ++ b)
    f(c)
  }
  */

  val dudVersionedHashFn: HashScope.VersionedHashFn =
    HashScope.VersionedHashFn.init(HashFn const 0)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object RandomData {

    val genHash: Gen[Option[Int]] = {
      val g = Gen.int.map(Some(_))
      Gen.chooseGen(Gen.int.option, List.fill(3)(g): _*)
    }


    val allHashScopes = AdtMacros.adtValues[HashScope]

    val genScope: Gen[HashScope] =
      Gen.chooseNE(allHashScopes)

    val genScopes: Gen[NonEmptyVector[HashScope]] =
      Gen.subset1(allHashScopes.whole).map(NonEmptyVector.force)

    def genScopeTo[A](scheme: HashScheme, g: Gen[A]): Gen[HashScope.To[A]] =
      Gen.subset(scheme.hashFns.scopeSet).flatMap(genScopeTo(_, g))

    def genScopeTo[A](scopes: Set[HashScope], g: Gen[A]): Gen[HashScope.To[A]] =
      Gen.traverse(scopes.toList)(s => g.map((s, _)))
        .map(e => HashScope.To(e.toMap))

    def genRecsByScheme(scheme: HashScheme): Gen[HashRecs.ByScheme] =
      genScopeTo(scheme, genHash).map(HashRecs.ByScheme(scheme, _))

    val genScheme: Gen[HashSchemeId => HashScheme] =
      for {
        scopes <- genScopes
        hashFns <- genScopeTo(scopes.whole.toSet, Gen pure dudVersionedHashFn)
      } yield HashScheme.withoutId(hashFns)

    val genEvolutionOps: StateGen[HashSchemes, NonEmptyVector[EvolutionOp]] =
      StateGen.genA(hs =>
        genScopes.flatMap(scopes =>
          Gen.traverse(scopes.whole)(s =>
            if (hs.latest.hashFns.contains(s))
              Gen.choose(
                EvolutionOp.Evolve(s -> HashFn.const(0)),
                EvolutionOp.Drop(s))
            else
              Gen.pure(
                EvolutionOp.Add(s -> HashFn.const(0))))
            .map(NonEmptyVector.force)))

    val genEvolve: StateGen[HashSchemes, Unit] =
      genEvolutionOps.flatMap(ops =>
        StateGen.mod(_.addEvolution(ops.head, ops.tail: _*)))

    val genHashSchemes: Gen[HashSchemes] =
      for {
        init <- genScheme
        evos <- Gen.chooseInt(4)
        hs   <- genEvolve.replicateM_(evos).exec(HashSchemes.initF(init))
      } yield hs

    def genHashRecs(schemes: HashSchemes): Gen[HashRecs] =
      for {
        ss <- Gen.chooseGen(Gen.subset(schemes.schemes.whole).map(_.toList), Gen.pure(schemes.latest :: Nil))
        rs <- Gen.traverse(ss)(genRecsByScheme)
      } yield HashRecs(rs)

//    val genHashRecs: Gen[HashRecs] =
//      genHashSchemes.flatMap(genHashRecs(_))

    def genBatchingTest(batch: Vector[HI] => Batches[Int]): Gen[BatchingTest] =
      for {
        hs <- genHashSchemes
        rs <- genHashRecs(hs).vector(0 to 63)
      } yield new BatchingTest(???, batch, rs)

  }
  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Manual {

    // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
    implicit val qweqweqwe: UnivEq[HashRecs] = UnivEq.force
    // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO

    implicit def autoScopeToX(s: HashScope): (HashScope, HashFn[Project]) =
      (s, HashFn const 0)

    val A = HashScope.ReqCodes
    val B = HashScope.CfgIssueTypes
    val C = HashScope.CfgFields
    val D = HashScope.GenericReqs
    val E = HashScope.UseCases

    // A B C1 -
    // - | C2 D
    val schemes =
      HashSchemes.init(A, B, C)
      .addEvolution(
        EvolutionOp.Drop(A),
        EvolutionOp.Evolve(C),
        EvolutionOp.Add(D))

    val batcher = Batcher[HI, Int](_._2, _._1, schemes)

    implicit class ScopeExt(private val s: HashScope) extends AnyVal {
      def >(i: Int) = s -> i
    }

    def hr(scheme: Int, xs: (HashScope, Option[Int])*): HashRecs = {
      val s = schemes.schemes.whole(scheme)
      assert(xs.map(_._1).forall(s.hashFns.scopeSet.contains))
      HashRecs(HashRecs.ByScheme(s, HashScope.To(xs.toMap)) :: Nil)
    }

    // HashRecs for both schemes
    def hr2(xs0: (HashScope, Option[Int])*)
           (xs1: (HashScope, Option[Int])*): HashRecs =
      HashRecs(hr(0, xs0: _*).values ::: hr(1, xs1: _*).values)

    implicit def autoSomeHashInts(h: Int): Option[Int] = Some(h)

    val forcePass = hr2(A -> None, B -> None, C -> None)(B -> None, C -> None, D -> None)

    val X1 = hr(0, A -> 3, B -> 4, C -> 5)
    val X2 = hr(1, B -> 4, C -> 7, D -> 6)
    val X12 = HashRecs(X1.values ::: X2.values)

    val Y1 = hr(0, A -> 11, B -> 12, C -> 13)
    val Y2 = hr(1, B -> 12, C -> 15, D -> 16)
    val Y12 = HashRecs(Y1.values ::: Y2.values)
  }

  override def tests = TestSuite {

    'batching {

//      'optimal - RandomData.genBatchingTest(batcher.optimal).mustSatisfyE(_.all)

//      'oneByOne - RandomData.genBatchingTest(batcher.oneByOne).mustSatisfyE(_.all)

      'manual {
        import Manual._

        def test(r1: HashRecs, r2: HashRecs)(expect1: HashRecs, expect2: HashRecs = HashRecs(null)): Unit = {
          val actual = batcher.optimal(Vector(r1 -> 1, r2 -> 2))
          if (expect2.values eq null) {
            assertEq("Batches", actual.map(_._1), List(List(1, 2)))
            val b :: Nil = actual
            assertEq("HashRecs", b._2, expect1)
          } else {
            assertEq("Batches", actual.map(_._1), List(List(1), List(2)))
            val b1 :: b2 :: Nil = actual
            assertEq("HashRecs", (b1._2, b2._2), (expect1, expect2))
  //          assertEq("Batch #2 HashRecs", b2._2, expect2)
  //          assertEq("Batch #1 HashRecs", b1._2, expect1)
          }
        }

        // Scheme 0 to 1
        // A dropped
        // B carry forward
        // C replaced

        // Scheme n
        // - replace
        // - add

        // no hash recs at all for an event = they've been manually removed = force pass
        // TODO Ensure that all events come with hash recs
        "0-0" - test(HashRecs.empty, HashRecs.empty)(forcePass)
        "1-0" - test(hr(0, A -> 5), HashRecs.empty)(hr(0, A -> 5), forcePass)
        "0-1" - test(HashRecs.empty, hr(0, A -> 5))(forcePass, hr(0, A -> 5))
        "2-0" - test(hr2(A -> 5)(B -> 7), HashRecs.empty)(hr2(A -> 5)(B -> 7), forcePass)
        "0-2" - test(HashRecs.empty, hr2(A -> 5)(B -> 7))(forcePass, hr2(A -> 5)(B -> 7))

        "1-1" - {
          'same {
            'replace - test(hr(0, A -> 5), hr(0, A -> 6))(hr(0, A -> 6))
            'add - test(hr(0, A -> 5), hr(0, B -> 6))(hr(0, A -> 5, B -> 6))
          }
          'diff {
            'drop - test(hr(0, A -> 5), hr(1, B -> 6))(hr(0, A -> 5), hr(1, B -> 6))
            'carry - test(hr(0, B -> 5), hr(1, C -> 6))(hr(0, B -> 5), hr(1, B -> 5, C -> 6))
            'replace - test(hr(0, C -> 5), hr(1, C -> 6))(hr(0, C -> 5), hr(1, C -> 6))
          }
        }

        "2-2" - test(X12, Y12)(Y12)
        "1-2" - test(X1, Y12)(X1, Y12)
        "2-1" - test(X12, Y2)(X12, Y2)
        // 1 -> 2
        // 2 -> 1
        // 2 -> 2

  //      'pairs {
  //        "0a_0a" - test(hr(0, A -> 5), hr(0, A -> 6))(hr(0, A -> 6))
  //        "0a_0b" - test(hr(0, A -> 5), hr(0, B -> 6))(hr(0, A -> 5, B -> 6))
  //        "0a_0C" - test(hr(0, A -> 5), hr(0, C -> 6))(hr(0, A -> 5, C -> 6))
  //        "0b_0a" - test(hr(0, B -> 5), hr(0, A -> 6))(hr(0, A -> 6, B -> 5))
  //        "0b_0b" - test(hr(0, B -> 5), hr(0, B -> 3))(hr(0, B -> 3))
  //        "0b_0C" - test(hr(0, B -> 5), hr(0, C -> 6))(hr(0, B -> 5, C -> 6))
  //        "0c_0a" - test(hr(0, C -> 5), hr(0, A -> 6))(hr(0, A -> 6, C -> 5))
  //        "0c_0b" - test(hr(0, C -> 5), hr(0, B -> 6))(hr(0, B -> 6, C -> 5))
  //        "0c_0C" - test(hr(0, C -> 5), hr(0, C -> 6))(hr(0, C -> 6))
  //      }
      }
    }

  }
}
