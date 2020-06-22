package shipreq.base.util.algorithm

import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import utest._

object GeneticEvolutionTest extends TestSuite {
  import GeneticEvolution.Binary._

  private implicit def chromosomeFromString(binaryString: String): Chromosome =
    MutableLargeBitSet.fromBinaryString(binaryString)

  private implicit final class StringTestOps(private val self: String) extends AnyVal {
    def c: Chromosome = self
  }

  private def assertChromosoneEq(name: String, a: Chromosome, e: Chromosome)(implicit l: Line): Unit =
    assertEq(name, a.toBinaryStr, e.toBinaryStr)

  private def assertChromosoneEq(a: Chromosome, e: Chromosome)(implicit l: Line): Unit =
    assertEq(a.toBinaryStr, e.toBinaryStr)

  private implicit def constRndInt(x: (Int, Int))(implicit l: Line): RndInt =
    (bound: Int) => {
      assertEq("RndInt.bound", bound, x._1)
      x._2
    }

  private implicit def constRndInt2(x: Int): RndInt =
    _ => x

  private val L0 = "0000000000000000000000000000000000000000000000000000000000000000"
  private val L1 = "1111111111111111111111111111111111111111111111111111111111111111"
  private val L2 = "1010101010101010101010101010101010101010101010101010101010101010"
  private val L3 = "0101010101010101010101010101010101010101010101010101010101010101"

  private val LX = List(L0, L1, L2, L3)
  private val LXX = for {a <- LX; b <- LX} yield a + b

  override def tests = Tests {

    "testHelpers" - {
      def test(bin: String, genes: Long*)(implicit l: Line): Unit = {
        val bits = bin.length
        val c = MutableLargeBitSet.fromArray(bits, genes.toArray)
        assertChromosoneEq("binary -> chromosone", bin: Chromosome, c)
        assertEq("chromosone -> binary", c.toBinaryStr, bin)
      }
      "0" - test("0", 0)
      "01" - test("01", 1)
      "10" - test("10", 2)
      "011" - test("011", 3)
      "L0" - test(L0, 0)
      "L1" - test(L1, -1)
    }

    "selection" - {
      val fs = Array.tabulate(4){n => val f = new Fitness; f.popIdx = n+10; f}
      val s = selection(fs)
      fs(0).score = 1
      fs(1).score = .7
      fs(2).score = .5
      fs(3).score = .1
      "full" - {
        val tests = List[(Double, Int)](
          .05 -> 3,
          .1 -> 3,
          .15 -> 2,
          .4 -> 2,
          .5 -> 2,
          .6 -> 1,
          .7 -> 1,
          .8 -> 0,
          .9 -> 0,
          1.0 -> 0,
        )
        for ((p, e) <- tests)
          assertEq(s"p=$p", s(p, -1), e)
      }
    }

    "crossover" - {
      def test(a: String, b: String, rnd: RndInt, expect: Chromosome)(implicit l: Line): Unit = {
        assertEq(a.length, b.length)
        val bits = a.length
        val actual = crossover(bits, rnd)(a, b)
        def bound = (bits - 1) << 1
        def desc = s"$bits/${rnd.nextInt(bound)}\n     a: $a\n     b: $b"
        assertChromosoneEq(desc, actual, expect)
      }

      "2/0" - test("00", "11", 2 -> 0, "01")
      "2/1" - test("00", "11", 2 -> 1, "10")

      "4/0" - test("000", "111", 4 -> 0, "001")
      "4/1" - test("000", "111", 4 -> 1, "011")
      "4/2" - test("000", "111", 4 -> 2, "110")
      "4/3" - test("000", "111", 4 -> 3, "100")

      "whole" - {
        for {
          l <- MutableLargeBitSetTest.bitsLens.filter(_ <= 127).filter(_ >= 1)
          a <- LXX
          b <- LXX
        } {
          val gaps = 127
          val c = l - 1
          val e = a.dropRight(l) + b.takeRight(l)
          test(a, b, c, e)
          test(b, a, c + gaps, e)
        }
      }
    }

    "run" - {
      assertEq(L0.c.fitnessToGoal(L0, L0.length), 0)
//      assertEq(L0.c.fitnessToGoal(L1, L0.length), 1)
//      assertEqWithTolerance(s"101${L0}010".c.fitnessToGoal(s"011${L0}011", 70), 3 / 70.0, 0.00000001)
//      assertEqWithTolerance("111001110".c.fitnessToGoal("000000000", 9), 6 / 9.0)

      assertEq("111".c.bit(0), true)
      assertEq("111".c.bit(1), true)
      assertEq("111".c.bit(2), true)
      assertEq("000".c.bit(0), false)
      assertEq("000".c.bit(1), false)
      assertEq("000".c.bit(2), false)
      assertEq("10".c.bit(0), false)
      assertEq("10".c.bit(1), true)
      assertEq("01".c.bit(0), true)
      assertEq("01".c.bit(1), false)

      def run(bits  : Int,
               target: Chromosome,
               config: Config): Chromosome = {
        GeneticEvolution.Binary.solveViaEvolution(
          bits    = bits,
          fitness = _.fitnessToGoal(target, bits),
          goal    = 0,
          config  = config,
        )
      }

      //      val goal = "10001000"
      val goal = "101" + L0
      val bits = goal.length
      val config = Config(
        popSize      = 128,
        eliteSize    = 64,
        maxGens      = 10000,
        mutationRate = .01,
        allowBrute   = true,
      )
      val result = run(bits, goal, config)
      assertChromosoneEq(result, goal)
    }
  }
}
