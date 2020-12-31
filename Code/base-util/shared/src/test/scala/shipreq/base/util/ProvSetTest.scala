package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import utest._
import nyaya.gen._
import nyaya.test.PropTest._
import shipreq.base.util.PartialOrder.Cmp

@nowarn
object ProvSetTest extends TestSuite {
  import PartialOrder.Cmp._
  import PartialOrder.ImplicitOps._
  import ProvSet.ProvEntry

  private object Internals {
    final case class K(id: String, rev: Int) {
      override val toString = s"$id$rev"
    }
    type V    = String
    type PE   = ProvEntry[K]
    type Prov = ArraySeq[ProvEntry[K]]

    implicit def univEqK: UnivEq[K] = UnivEq.derive
    implicit val keyOrder = keyedInt((_: K).id, (_: K).rev)
    val module = ProvSet.Module[K, V](_.into, _.key.toString < _.key.toString)
    import module.{ProvSet, empty}

    val parseK: String => K = {
      val regex = "^([A-Za-z]+)(\\d+)$".r
      s => {
        val regex(id, rev) = s
        K(id, rev.toInt)
      }
    }

    private val regexPE = "^(?:([A-Za-z0-9]+)(?:->|<))?([A-Za-z0-9]+)$".r

    private def parsePE(k: K, s: String): PE =
      s match {
        case regexPE(from, to) =>
          ProvEntry(
            from = Option(from).fold(k)(parseK),
            to   = parseK(to))
      }

    private val regexPS1 = "^\\{(.+?)}:\\{(.+)}$".r
    private val regexPS2 = "^\\{(.+?)}$".r
    implicit def strToProvSet(s: String): ProvSet = {

      def singleVal(id: String, prov: String): ProvSet = {
        val k = parseK(id)
        val provs = prov.split(',').iterator.filter(_ != "").map(parsePE(k, _)).toSet
        module(Map(k -> id), provs)
      }

      s match {
        case regexPS1(valuesStr, provStr) =>
          val values = valuesStr.split(',').iterator.map(s => parseK(s) -> s).toMap
          val provs = provStr.split(',').iterator.filter(_ != "").map(parsePE(null, _)).toSet
          module(values, provs)

        case regexPS2(valuesStr) =>
          val values = valuesStr.split(',').iterator.map(s => parseK(s) -> s).toMap
          module(values, Set.empty)

        case _ =>
          s.indexOf(':') match {
            case -1 => singleVal(s, "")
            case n  => singleVal(s.take(n), s.drop(n + 1))
          }
      }

    }

    //    implicit def strSeqToEntry(ss: Seq[String]): Seq[Entry] =
//      ss.map(strToEntry)

    def assertAdd(i: ProvSet, j: ProvSet)(expect: ProvSet)(implicit l: Line): Unit = {
      i.assertProps()
      j.assertProps()
      assertEq(s"$i + $j", i ++ j, expect)
      assertEq(s"$j + $i [reverse]", j ++ i, expect)
    }

//    def assertConsolidation(inputs: Entry*)(expect: Entry*)(implicit l: Line): Unit = {
//      val actual = module.consolidate(inputs: _*).repr
////      assertSet(actual)(expect: _*)
//      assertEq(
//        actual = actual.toList.sortBy(_.toString),
//        expect = expect.toList.sortBy(_.toString))
//    }

    def assertCmp[A](x: A, y: A)(expect: Cmp)(implicit l: Line, p: PartialOrder[A]): Unit = {
      assertEq(s"$x cmp $y", p(x, y), expect)
      assertEq(s"$y cmp $x [reverse]", p(y, x), expect.flip)
    }
  }

  // ===================================================================================================================
  import Internals._
  import module.ProvSet

  override def tests = Tests {

    "props" - {
      val Laws = new ProvSet.Laws(module)
      import Laws._

      val range = 3
      val size = 2

      val genId  = Gen.choose_!(('A' to 'Z').take(range).map(_.toString))
      val genRev = Gen.chooseInt(range)
      val genK   = Gen.lift2(genId, genRev)(K.apply)

      val getPE: Gen[ProvEntry[K]] =
        for {
          k <- genK
          j <- genK.map(j => Option.when(k isSeparateTo j)(j)).optionGet
        } yield ProvEntry(k, j)

      val genPS: Gen[ProvSet] =
        for {
          prov       <- getPE.set(0 to size)
          provSize    = prov.size
          valueKeys <- genK.arraySeq(0 to provSize)
        } yield {
          val values = valueKeys.iterator.map(k => k -> s"${k.id}${k.rev}").toMap
          module(values, prov).pruneValues
        }

      val gen: Gen[Input] =
        Gen.lift3(genPS, genPS, genPS)(ProvSet.Laws.Input(_, _, _))

//      import japgolly.microlibs.stdlib_ext.StdlibExt._
//      gen.withSeed(0).samples().take(100).drain()
      laws.mustBeSatisfiedBy(gen.withSeed(6))
    }

    "manual" - {
      "basic" - {
        "gt"  - assertAdd("A2", "A3")("A3")
        "lt"  - assertAdd("A2", "A1")("A2")
        "eq"  - assertAdd("A2", "A2")("A2")
        "sep" - assertAdd("A2", "B3")("{A2,B3}")
      }
      "prov" - {
        "eq"     - assertAdd("A2:B3<A1", "B3")("A2:B3<A1")
        "gt"     - assertAdd("A2:B3<A1", "B2")("A2:B3<A1")
        "lt"     - assertAdd("A2:B3<A1", "B4")("{A2,B4}:{B3<A1}")
        "sep"    - assertAdd("A2:B3<A1", "C2:B2<C1")("{A2,C2}:{B3<A1,B2<C1}")
        "merge"  - assertAdd("A2:B3<A1", "B2:C6<B1")("{A2}:{B3<A1,C6<B1}")
        "cycle1" - assertAdd("A1:B1<A1", "B1:A1<B1")("{B1}:{A1<B1,B1<A1}")
        "cycle2" - assertAdd("B2:A1<B1,C0<A0", "C1:B2<C0")("C1:A1<B1,C0<A0,B2<C0")
        "cycle3" - {
          val expect: ProvSet = "{B1}:{A1<C0,A2<B1,C0<A1}"
          implicit val po = expect.partialOrder.contramap(parseK)
          "A1_A2" - assertCmp("A1", "A2")(Lesser)
          "A1_B1" - assertCmp("A1", "B1")(Lesser)
          "A1_C0" - assertCmp("A1", "C0")(Lesser)
          "A2_B1" - assertCmp("A2", "B1")(Lesser)
          "A2_C0" - assertCmp("A2", "C0")(Greater)
          "B1_C0" - assertCmp("B1", "C0")(Greater)
          "add"   - assertAdd("{B1}:{A1<C0,A2<B1}", "{A2}:{C0<A1}")(expect)
        }
        "cycle4" - {
          val expect: ProvSet = "{D2}:{B1<C0,A2<B1,C0<B1,C1<D1}"
          implicit val po = expect.partialOrder.contramap(parseK)
          "A1_A2" - assertCmp("A1", "A2")(Lesser)
          "A1_B1" - assertCmp("A1", "B1")(Lesser)
          "A1_C0" - assertCmp("A1", "C0")(Lesser)
          "A1_C1" - assertCmp("A1", "C1")(Lesser)
          "A1_D1" - assertCmp("A1", "D1")(Lesser)
          "A2_B1" - assertCmp("A2", "B1")(Lesser)
          "A2_C0" - assertCmp("A2", "C0")(Lesser)
          "A2_C1" - assertCmp("A2", "C1")(Lesser)
          "A2_D1" - assertCmp("A2", "D1")(Lesser)
          "B1_C0" - assertCmp("B1", "C0")(Lesser)
          "B1_C1" - assertCmp("B1", "C1")(Lesser)
          "B1_D1" - assertCmp("B1", "D1")(Lesser)
          "C0_C1" - assertCmp("C0", "C1")(Lesser)
          "C0_D1" - assertCmp("C0", "D1")(Lesser)
          "C1_D1" - assertCmp("C1", "D1")(Lesser)
          "add"   - assertAdd("{B1}:{B1<C0,A2<B1,C0<B1}", "{D2}:{C1<D1}")(expect)
        }
      }
    }

  }
}
