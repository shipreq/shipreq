package shipreq.webapp.base.hash2

import japgolly.microlibs.nonempty.NonEmptyVector
import nyaya.gen._
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import scala.annotation.tailrec
import scala.collection.SeqLike
import scalaz.syntax.applicative._
import scalaz.{-\/, \/-}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen._

object Hash3Tests extends TestSuite {
  import HashTestUtil._

  object Ah {
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

  override def tests = TestSuite {

    'prop {
//      Ah.genTester.bugHunt(4)(Prop.eval(_.all))(DefaultSettings.propSettings.setDebug)
      Ah.genTester.mustSatisfyE(_.all)(DefaultSettings.propSettings.setSampleSize(7 * 100))
    }

  }
}
