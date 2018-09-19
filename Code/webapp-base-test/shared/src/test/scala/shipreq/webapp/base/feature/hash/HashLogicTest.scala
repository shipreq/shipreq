package shipreq.webapp.base.feature.hash

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.gen._
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import scala.annotation.tailrec
import scala.collection.SeqLike
import scalaz.{-\/, \/-}
import scalaz.syntax.applicative._
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen._
import shipreq.webapp.base.data.Project
import HashTestUtil._

object HashLogicTest extends TestSuite {

  final class BatcherProps[Scope: UnivEq, Data](batcher: HashLogic.Batcher[Scope, Data, (EvoHashModule.HashRecs[Scope, Data], Int), Int],
                                                batch: Vector[(EvoHashModule.HashRecs[Scope, Data], Int)] => HashLogic.Batches[Scope, Data, Int],
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

    private def noEmptyBatches =
      E.test("No empty batches", batches.forall(_.elements.nonEmpty))

//    They could be wiped from the DB
//    private def noEmptyRecs =
//      E.test("No empty recs", batches.forall(_.recs.nonEmpty))

    private def noEmptyRecsByScheme =
      E.test("No empty recs by scheme", batches.forall(_.recs.values.forall(_.nonEmpty)))

    lazy val all =
      (elements & latestHashRecIntact & noEmptyBatches & noEmptyRecsByScheme)
        .rename("Batching props")
  }

  object BatcherTest extends EvoHashModule.Types[Char, Unit] {
    type HI = (HashRecs, Int)

    val rnd = RandomHashData[Scope, Data](NonEmptyVector force 'A'.to('M').to)

    def apply(batch: Batcher[HI, Int] => Vector[HI] => Batches[Int]): Gen[BatcherProps[Scope, Data]] =
      for {
        hs ← rnd.genSchemes
        b  = HashLogic.Batcher[Scope, Data, HI, Int](_._2, _._1, hs)
        rs ← rnd.genHashRecs(hs).vector(0 to 63)
      } yield new BatcherProps(b, batch(b), rs)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Manual {
    import FakeScope._
    import FakeModule._

    val dudVersionedHashFn: VersionedHashFn =
      VersionedHashFn.init(HashFn const 0)

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object EventStreamSimulation {
    import FakeModule._
    val R = RandomHashData[FakeScope, FakeData](FakeScope.all)

    val genFakeValue: Gen[Int] =
      Gen.chooseInt(100000, 999999)

    val genEvent: Gen[FakeEvent] =
      Gen.chooseGen(
        R.genScope.mapTo(genFakeValue)(1 to FakeScope.all.length).map(FakeEvent),
        R.genScope.mapTo(genFakeValue)(1).map(FakeEvent))

    def genVerifiedEvent(schemes: NonEmptyVector[Scheme]): StateGen[FakeData, FakeVerifiedEvent] =
      StateGen.tailrec(s1 =>
        genEvent.map { e =>
          val s2 = e(s1)
          val hrs = schemes.iterator.map(_.changes(s1, s2)).filter(_.nonEmpty)
          if (hrs.isEmpty)
            -\/(s1)
          else
            \/-((s2, FakeVerifiedEvent(e, hrs.reduce(_ ++ _))))
        }
      )

    val emptyData = FakeData(0, 0, 0, 0)

    def genVerifiedEventStream(genSchemes: Gen[NonEmptyVector[Scheme]], d0: FakeData = emptyData)(implicit ss: SizeSpec): Gen[(FakeData, List[FakeVerifiedEvent])] = {
      val genVE: StateGen[FakeData, FakeVerifiedEvent] =
        StateGen.genA((_: FakeData) => genSchemes).flatMap(genVerifiedEvent)
      ss.gen1.flatMap(sz => genVE.replicateM(sz).run(d0))
    }

    def sizedGroups[C[X] <: SeqLike[X, C[X]], A](input: C[A])(implicit ss: SizeSpec): Gen[Vector[C[A]]] = { // TODO Add to Nyaya
      val size = ss match {
        case x@ SizeSpec.Default => x.gen
        case x: SizeSpec.Exactly => x.gen
        case SizeSpec.OneOf(ns)  => Gen.chooseIndexed_!(ns)
      }
      Gen { ctx =>
        val groups = Vector.newBuilder[C[A]]
        @tailrec def go(as: C[A]): Unit =
          if (as.nonEmpty) {
            val n = size.run(ctx)
            groups += as.take(n)
            go(as.drop(n))
          }
        go(input)
        groups.result()
      }
    }

    val genTester: Gen[Tester] = {
      val allSchemes = schemeRegistry

      def singleScheme(scheme: Scheme): Gen[NonEmptyVector[Scheme]] =
        Gen pure NonEmptyVector.one(scheme)

      val chooseSchemes1: Gen[NonEmptyVector[Scheme]] =
        singleScheme(allSchemes.schemes.head)

      val chooseSchemes2: Gen[NonEmptyVector[Scheme]] =
        Gen.chooseGen(
          Gen.subset1(allSchemes.schemes.whole).map(NonEmptyVector.force),
          Gen.chooseNE(allSchemes.schemes).map(NonEmptyVector.one))

      val chooseSchemes3: Gen[NonEmptyVector[Scheme]] =
        singleScheme(allSchemes.latest)

      val Total = 64
      val Tests = 16

      for {
        mono       <- Gen.chooseInt(24)
        poly        = Total - mono * 2
        d0          = emptyData
        (d1, ves1) <- genVerifiedEventStream(chooseSchemes1, d0)(mono)
        (d2, ves2) <- genVerifiedEventStream(chooseSchemes2, d1)(poly)
        (d3, ves3) <- genVerifiedEventStream(chooseSchemes3, d2)(mono)
        d = d3
        ves = (Vector.newBuilder[FakeVerifiedEvent] ++= ves1 ++= ves2 ++= ves3).result()
        tests <- sizedGroups(ves)(1 until Total).list(Tests)
      } yield new Tester(d, tests)
    }

    final val Debug = false

    val scopesInBothSchemes: List[FakeScope] =
      List(FakeScope.B, FakeScope.C)

    final class Tester(endResult: FakeData, _testData: List[Vector[Vector[FakeVerifiedEvent]]]) {
      val E = EvalOver(this)

      val allInOne = _testData.head.flatten
      val testData = Vector(allInOne) :: _testData

      if (Debug) {
        println("=" * 120)
        allInOne.foreach(println)
        println("=" * 120)
        batcherVE.optimal(allInOne).foreach(b => println(s"${b.elements.size} elements: ${b.recs}"))
        println("=" * 120)
      }

      type Batches = batcherVE.Batches // import batcherVE._ // stupid IntelliJ

      override def toString =
        s"${testData.map(_.map(_.length).mkString("-")).mkString("{", ", ", "}")}"

      private def testBatcher(batchFn: Vector[FakeVerifiedEvent] => Batches) =
        E.forall(testData) { testInstance =>
          var d = emptyData
          var eval: EvalL = null
          def fail(f: EvalL): Unit = eval = Option(eval).fold(f)(_ & f)
          testInstance.foreach { partitions =>

            // Consolidate each group
            val batches = batchFn(partitions)

            batches.foreach { batch =>
              if (eval eq null) {

                // Apply events
                val d1 = d
                val d2 = batch.elements.foldLeft(d1)((dx, e) => e(dx))
                d = d2

                // Ensure passes validation
                val result = HashLogic.validate(batch.recs, before = d1, current = d2)
                if (result.nonEmpty)
                  fail(E.fail("Validation must pass", s"$d1 ⇒ $d2 ## ${result.mkString(", ")}"))

                if (Debug) {
                  println(if (result.isEmpty) s"${Console.GREEN}${Console.BOLD}Validation Pass${Console.RESET}" else s"${Console.RED}${Console.BOLD}Validation Failure${Console.RESET}")
                  println(s"Before: $d1")
                  println(s"After : $d2")
                  println(s"Recs  : ${batch.recs.mkString(", ")}")
                  if (result.nonEmpty) {
                    println("Failures:")
                    result.map("- " + _).foreach(println)
                  }
                  println("Events:")
                  batch.elements.map("- " + _).foreach(println)
                  println()
                }


                // Ensure that modification fails validation
                for (s <- scopesInBothSchemes) {
                  val result2 = HashLogic.validate(batch.recs, before = d1, current = d2.mod(s, _ + 1))
                  if (result2.isEmpty)
                    fail(E.fail("Fake modification should fail validation", batch.recs.toString))
                }
              }
            }

          }

          if (eval ne null) eval else E.pass
        }

      lazy val optimal  = testBatcher(batcherVE.optimal).rename("optimal batching")
      lazy val oneByOne = testBatcher(batcherVE.oneByOne).rename("one-by-one batching")
      lazy val all      = optimal & oneByOne
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  override def tests = Tests {

    'batcher {

      'oneByOne - BatcherTest(_.oneByOne).mustSatisfyE(_.all)

      'optimal - BatcherTest(_.optimal).mustSatisfyE(_.all)

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

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

        implicit def univEqVersionedHashFn: UnivEq[VersionedHashFn] = UnivEq.force

        assertEq(actual2, expect2)
      }

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

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    'eventStreamSim {
      EventStreamSimulation.genTester.mustSatisfyE(_.all)(DefaultSettings.propSettings.setSampleSize(7 * 16))
    }

  }
}
