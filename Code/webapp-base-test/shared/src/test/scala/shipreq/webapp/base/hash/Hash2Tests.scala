package shipreq.webapp.base.hash

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.gen._
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.Project

object Hash2Tests extends TestSuite {
  import EvoHashModule.Schemes
  import HashLogic.{Batcher, Batches}
  import HashTestUtil._

  class BatchingTest[Scope: UnivEq, Data](batcher: Batcher[Scope, Data, (EvoHashModule.HashRecs[Scope, Data], Int), Int],
                                          batch: Vector[(EvoHashModule.HashRecs[Scope, Data], Int)] => Batches[Scope, Data, Int],
                                          hs: Vector[EvoHashModule.HashRecs[Scope, Data]]) {
    private val E = EvalOver(hs)

    private val his = hs.zipWithIndex
    private val batches = batch(his)

    private def elements =
      E.equal("consolidated batched elements = unbatched elements",
        batches.flatMap(_.elements).toVector,
        his.map(_._2))

    private def latestHashRecIntact = {
      var p = E.pass
      for {
        expects           ← hs.lastOption
        actuals           = batches.last.recs
        (scheme, byScope) ← expects
        subActual         = actuals.get(scheme)
        (scope, hash)     ← byScope
        actualValue       = subActual.flatMap(_ get scope)
        expectValue       = Some(hash)
      } if (actualValue ≠ expectValue)
        p &= E.fail("The latest input HashRec, should be intact in the last batch",
          s"$scheme $scope: $actualValue ≠ $expectValue}")
      p
    }

//    private def allScopesPerScheme =
//      E.forall(batches.flatMap(_.recs)) { case (scheme, byScope) =>
//        E.equal(s"$scheme scopes", byScope.keySet, scheme.hashFns.keySet)
//      }.rename("All scopes exist per scheme")

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

  object GenBatchingTest {
    type Scope = Char
    type Data = Unit
    type HashRecs = EvoHashModule.HashRecs[Scope, Data]
    type HI = (HashRecs, Int)

    val rnd = RandomHashData[Scope, Data](NonEmptyVector force 'A'.to('M').to)

    def apply(batch: Batcher[Scope, Data, HI, Int] => Vector[HI] => Batches[Scope, Data, Int]): Gen[BatchingTest[Scope, Data]] =
      for {
        hs <- rnd.genSchemes
        b = Batcher[Scope, Data, HI, Int](_._2, _._1, hs)
        rs <- rnd.genHashRecs(hs).vector(0 to 63)
      } yield new BatchingTest(b, batch(b), rs)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Manual {
    import FakeScope._
    import FakeModule._

    val dudVersionedHashFn: VersionedHashFn =
      VersionedHashFn.init(HashFn const 0)

    // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
    implicit val qweqweqwe: UnivEq[HashRecs] = UnivEq.force
    // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO

    implicit def autoScopeToX(s: Scope): (Scope, HashFn[Project]) =
      (s, HashFn const 0)

    implicit class ScopeExt(private val s: Scope) extends AnyVal {
      def >(i: Int) = s -> i
    }

    def hr(scheme: Int, xs: (Scope, Option[Int])*): HashRecs = {
      val s = schemeRegistry.schemes.whole(scheme)
      assert(xs.map(_._1).forall(s.hashFns.contains))
      Map(s -> xs.toMap)
    }

    // HashRecs for both schemes
    def hr2(xs0: (Scope, Option[Int])*)
           (xs1: (Scope, Option[Int])*): HashRecs =
      hr(0, xs0: _*) ++ hr(1, xs1: _*)

    implicit def autoSomeHashInts(h: Int): Option[Int] = Some(h)

    val forcePass = hr2(A -> None, B -> None, C -> None)(B -> None, C -> None, D -> None)

    val X1 = hr(0, A -> 3, B -> 4, C -> 5)
    val X2 = hr(1, B -> 4, C -> 7, D -> 6)
    val X12 = X1 ++ X2

    val Y1 = hr(0, A -> 11, B -> 12, C -> 13)
    val Y2 = hr(1, B -> 12, C -> 15, D -> 16)
    val Y12 = Y1 ++ Y2

    val none = Option.empty[Int]
  }

  override def tests = TestSuite {

    'batching {

      'oneByOne - GenBatchingTest(_.oneByOne).mustSatisfyE(_.all)

      'optimal - GenBatchingTest(_.optimal).mustSatisfyE(_.all)

      'manual {
        import Manual._
        import FakeScope._
        import FakeModule._

        def test(r1: HashRecs, r2: HashRecs)(expect1: HashRecs, expect2: HashRecs = null): Unit = {
          val actual = batcher.optimal(Vector(r1 -> 1, r2 -> 2))
          if (expect2 eq null) {
            assertEq("Batches", actual.map(_.elements), List(List(1, 2)))
            val b :: Nil = actual
            assertEq("HashRecs", b.recs, expect1)
          } else {
            assertEq("Batches", actual.map(_.elements), List(List(1), List(2)))
            val b1 :: b2 :: Nil = actual
            assertEq("HashRecs", (b1.recs, b2.recs), (expect1, expect2))
          }
        }

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
            'replace - test(hr(0, C -> 5), hr(1, C -> 6))(hr(0, C -> 5), hr(1, C -> 6))

            // This doesn't make sense actully, the 2nd batch doesn't change B so there's no need to carry over B from
            // the first batch. The lack of a B hash in batch 2 already means "no change to B"
            // 'carry - test(hr(0, B -> 5), hr(1, C -> 6))(hr(0, B -> 5), hr(1, B -> 5, C -> 6))
          }
        }

        "2-2" - test(X12, Y12)(Y12)
        "1-2" - test(X1, Y12)(X1, Y12)
        "2-1" - test(X12, Y2)(X12, Y2)
      }
    }

    'validate {
      import Manual._
      import FakeScope._
      import FakeModule._

      def testPass(before: FakeData, current: FakeData, recs: HashRecs): Unit =
        _test(before, current, recs)()

      def testFail(before: FakeData, current: FakeData, recs: HashRecs)(e: (Int, List[VersionedHashFn]), ee: (Int, List[VersionedHashFn])*): Unit =
        _test(before, current, recs)(e +: ee: _*)

      def _test(before: FakeData, current: FakeData, recs: HashRecs)(expectedErrors: (Int, List[VersionedHashFn])*): Unit = {
        val actual: List[HashDiscrepancy] =
          HashLogic.validate(recs, before = before, current = current)

        val actual2: Map[Scheme, Set[VersionedHashFn]] =
          actual.groupBy(_.scheme).mapValuesNow(_.map(d => d.scheme.hashFns(d.scope)).toSet)

        val expect2: Map[Scheme, Set[VersionedHashFn]] =
          expectedErrors.map(_.map1(schemeRegistry.schemes.whole.apply)).groupBy(_._1).mapValuesNow(_.flatMap(_._2).toSet)

        // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
        implicit val qwefsdafga32wewe: UnivEq[VersionedHashFn] = UnivEq.force

        assertEq(actual2, expect2)
      }

      // A1 B1 C1 -
      //  -  | C2 D1

      'allChanged {
        val x = FakeData(1, 2, 3, 4)
        val y = FakeData(9, 8, 7, 6)
        'empty  - testPass(x, y, HashRecs.empty) // think about it...
        'all0   - testPass(x, y, hr(0, A -> 109, B -> 108, C -> 107))
        'all1   - testPass(x, y, hr(1, B -> 108, C -> 207, D -> 106))
        'all01  - testPass(x, y, hr2(A -> 109, B -> 108, C -> 107)(B -> 108, C -> 207, D -> 106))
        'bad0   - testFail(x, y, hr(0, A -> 109, B -> 0, C -> 107))(0 -> List(B1))
        'bad1   - testFail(x, y, hr(1, B -> 108, C -> 0, D -> 0))(1 -> List(C2, D1))
        'miss0  - testFail(x, y, hr(0, A -> 109, C -> 107))(0 -> List(B1))
        'miss1  - testFail(x, y, hr(1, D -> 106))(1 -> List(B1, C2))
        'fail01 - testFail(x, y, hr2(A -> 0, B -> 108)(C -> 207))(0 -> List(A1, C1), 1 -> List(B1, D1))
      }

      'noneChanged {
        val x = FakeData(1, 2, 3, 4)
        'empty  - testPass(x, x, HashRecs.empty)
        'all0   - testPass(x, x, hr(0, A -> 101, B -> 102, C -> 103))
        'all1   - testPass(x, x, hr(1, B -> 102, C -> 203, D -> 104))
        'all01  - testPass(x, x, hr2(A -> 101, B -> 102, C -> 103)(B -> 102, C -> 203, D -> 104))
        'bad01  - testFail(x, x, hr2(A -> 101, B -> 0, C -> 103)(B -> 102, C -> 0, D -> 104))(0 -> List(B1), 1 -> List(C2))
        'miss0  - testPass(x, x, hr(0, A -> 101))
        'miss01 - testPass(x, x, hr2(A -> 101)(B -> 102))
        'miss01 - testPass(x, x, hr2(B -> 102)(C -> 203))
      }

      'forcePass {
        'pass0 - testPass(FakeData(1, 2, 3, 4), FakeData(9, 8, 7, 6), hr(0, A -> none, B -> none, C -> 107))
        'bad0  - testFail(FakeData(1, 2, 3, 4), FakeData(9, 8, 7, 6), hr(0, A -> none, B -> none, C -> 0))(0 -> List(C1))
      }
    }

    /*
    Gen schemes
    Gen events that affect (1-n fields)
      - foreach event, choose 0-n schemes, calculate diff (discard if no diff)

    Partition [(event x recs)] by into [0..3] groups
    Consolidate each group
    Ensure each group applies and passes validation

    Use OneByOne
    Ensure each group applies and passes validation
     */

  }
}
