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

//      val genE: Gen[Entry] =
//        for {
//          k    <- genK
//          prov <- genId.set(0 to size).map(_ - k.id).flatMap(Gen.traverse(_)(id => genRev.map(K(id, _))))
//        } yield {
//          val v = s"${k.id}${k.rev}"
//          module.entry(k, v, prov)
//        }

      // TODO Prop test by op

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

//          @tailrec
//          def go(prov: Set[ProvEntry[K]], provSize: Int): ProvSet = {
//            val s = module(values.take(provSize), prov)
//            val e = PartialOrder.Props.eval(s.allKeys)(s.partialOrder)
//            if (e.success)
//              s.pruneValues
//            else
//              go(prov.tail, provSize - 1)
//          }
//
//          go(prov, provSize)

          module(values, prov).pruneValues
        }

      val gen: Gen[Input] =
        Gen.lift3(genPS, genPS, genPS)(ProvSet.Laws.Input(_, _, _))

//      import japgolly.microlibs.stdlib_ext.StdlibExt._
//      gen.withSeed(0).samples().take(100).drain()
      laws.mustBeSatisfiedBy(gen.withSeed(6))
    }

//    "partialOrderE" - {
//      def test(x: Entry, y: Entry)(expect: Cmp)(implicit l: Line): Unit = {
//        assertEq(s"$x cmp $y", module.partialOrderE(x, y), expect)
//        assertEq(s"$y cmp $x", module.partialOrderE(y, x), expect.flip)
//      }
//      "eq"      - test("A2", "A2")(Equal)
//      "lt"      - test("A1", "A2")(Lesser)
//      "sep"     - test("A2", "B2")(Separate)
//      "prov12"  - test("A0:B1", "B2")(Separate)
//      "prov22"  - test("A0:B2", "B2")(Greater)
//      "prov32"  - test("A0:B3", "B2")(Greater)
//      "sepP"    - test("A0:C1", "B2:D1")(Separate)
//      "mutual1" - test("A2:B2", "B2:A2")(Lesser) // by key for commutativity
//      "mutual2" - test("A2:B9", "B2:A9")(Lesser) // by key for commutativity
//      "misc1"   - test("A0", "C2:A2")(Lesser)
//    }

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
//        "merge<" - assertAdd("A2:B3<A1,C5<A1", "B2:C6<B1")("{A2}:{B3<A1,C6<B1}")
//        "merge>" - assertAdd("A2:B3<A1,C5<A1", "B2:C4<B1")("{A2}:{B3<A1,C5<A1}")
        "cycle1" - assertAdd("B2:A1<B1,C0<A0", "C1:B2<C0")("C1:A1<B1,C0<A0,B2<C0")
        "cycle2" - {
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
        "cycle3" - {
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

//      "misc" - {
//        "1" - assertAdd("A0", "C2:A2<C1")("C2:A2<C1")
////        "2" - assertAdd("B1:A0<B0,C2<B0", "C1:B2<C0")("{C0}:{A0<B0,C2<B0,B2<C0}")
//        "3" - {
//          val a   = "A1:B0"
//          val b   = "C0:A0"
//          val c   = "A0:B1"
//          val ab  = "{A1,C0}:{C0->A0,A1->B0}"
//          val ac  = "{A1}:{A1->B0,A0->B1}"
//          val bc  = "{C0}:{C0->A0,A0->B1}"
//          val abc = "{A1,C0}:{A0->B1,A1->B0,C0->A0}"
//          "ab"  - assertAdd(a, b)(ab)
////          "ac"  - assertAdd(a, c)(ac)
////          "bc"  - assertAdd(b, c)(bc)
////          "abc" - assertAdd(ab, c)(abc)
////          "acb" - assertAdd(ac, b)(abc)
//        }
//      }
    }

  }
}
