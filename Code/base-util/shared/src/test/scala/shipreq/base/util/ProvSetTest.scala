package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import utest._
import nyaya.gen._
import nyaya.test.PropTest._
import shipreq.base.util.PartialOrder.Cmp

object ProvSetTest extends TestSuite {
  import PartialOrder.Cmp._
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
    import module.{Entry, empty}

    private val parseK: String => K = {
      val regex = "^([A-Za-z]+)(\\d+)$".r
      s => {
        val regex(id, rev) = s
        K(id, rev.toInt)
      }
    }

    private val regexPE = "^([A-Za-z0-9]+)(?:<-([A-Za-z0-9]+))?$".r

    private def parsePE(k: K, s: String): PE = {
      val regexPE(height, src) = s
      ProvEntry(
        src    = Option(src).fold(k)(parseK),
        height = parseK(height))
    }

    def entry(id: String, prov: String = ""): Entry = {
      val k = parseK(id)
      val provs = prov.split(',').iterator.filter(_.nonEmpty).map(parsePE(k, _)).toSet
      module.entry(k, id, provs)
    }

    implicit def strToEntry(s: String): Entry =
      s.indexOf(':') match {
        case -1 => entry(s)
        case n  => entry(s.take(n), s.drop(n + 1))
      }

//    implicit def strSeqToEntry(ss: Seq[String]): Seq[Entry] =
//      ss.map(strToEntry)

    def assertAdd(i: Entry, j: Entry)(expect: Entry*)(implicit l: Line): Unit = {
      val e =  expect.toSet
      assertEq(s"$i + $j", (empty + i + j).repr, expect = e)
      assertEq(s"$j + $i [reverse]", (empty + j + i).repr, expect = e)
    }

    def assertConsolidation(inputs: Entry*)(expect: Entry*)(implicit l: Line): Unit = {
      val actual = module.consolidate(inputs: _*).repr
//      assertSet(actual)(expect: _*)
      assertEq(
        actual = actual.toList.sortBy(_.toString),
        expect = expect.toList.sortBy(_.toString))
    }
  }

  // ===================================================================================================================
  import Internals._
  import module.Entry

  override def tests = Tests {

    /*
    "props" - {
      val Laws = new ProvSet.Laws(module)
      import Laws._

      val range = 3
      val size = 2

      val genId  = Gen.choose_!(('A' to 'Z').take(range).map(_.toString))
      val genRev = Gen.chooseInt(range)
      val genK   = Gen.lift2(genId, genRev)(K.apply)

      val genE: Gen[Entry] =
        for {
          k    <- genK
          prov <- genId.set(0 to size).map(_ - k.id).flatMap(Gen.traverse(_)(id => genRev.map(K(id, _))))
        } yield {
          val v = s"${k.id}${k.rev}"
          module.entry(k, v, prov)
        }

      val genPS: Gen[ProvSet] =
        genE.list(0 to size).map(es => module.consolidate(es : _*))

      val gen: Gen[Input] =
        Gen.lift3(genPS, genPS, genPS)(ProvSet.Laws.Input(_, _, _))

//      import japgolly.microlibs.stdlib_ext.StdlibExt._
//      gen.withSeed(0).samples().take(100).drain()
//      laws.mustBeSatisfiedBy(gen.withSeed(0))
    }
    */

    "partialOrderE" - {
      def test(x: Entry, y: Entry)(expect: Cmp)(implicit l: Line): Unit = {
        assertEq(s"$x cmp $y", module.partialOrderE(x, y), expect)
        assertEq(s"$y cmp $x", module.partialOrderE(y, x), expect.flip)
      }
      "eq"      - test("A2", "A2")(Equal)
      "lt"      - test("A1", "A2")(Lesser)
      "sep"     - test("A2", "B2")(Separate)
      "prov12"  - test("A0:B1", "B2")(Separate)
      "prov22"  - test("A0:B2", "B2")(Greater)
      "prov32"  - test("A0:B3", "B2")(Greater)
      "sepP"    - test("A0:C1", "B2:D1")(Separate)
      "mutual1" - test("A2:B2", "B2:A2")(Lesser) // by key for commutativity
      "mutual2" - test("A2:B9", "B2:A9")(Lesser) // by key for commutativity
      "misc1"   - test("A0", "C2:A2")(Lesser)
    }

    "manual" - {
      "basic" - {
        "gt"  - assertAdd("A2", "A3")("A3")
        "lt"  - assertAdd("A2", "A1")("A2")
        "eq"  - assertAdd("A2", "A2")("A2")
        "sep" - assertAdd("A2", "B3")("A2", "B3")
      }
      "prov" - {
        "eq"     - assertAdd("A2:B3", "B3")("A2:B3")
        "gt"     - assertAdd("A2:B3", "B2")("A2:B3")
        "lt"     - assertAdd("A2:B3", "B4")("A2:B3", "B4")
        "sep"    - assertAdd("A2:B3", "C1:B2")("A2:B3", "C1:B2")
        "mergeA" - assertAdd("A2:B3", "B2:C6")("A2:B3,C6")
        "mergeL" - assertAdd("A2:B3,C8", "B2:C6")("A2:B3,C8")
        "mergeG" - assertAdd("A2:B3,C1", "B2:C4")("A2:B3,C4")
      }
      "misc" - {
//        "1" - assertAdd("A0", "C2:A2")("C2:A2")
//        "2" - assertAdd("B1:A0,C2", "C0:B2")("C0:A0,B2,C2")
        "3" - {
          val a   = "A1:B0": Entry
          val b   = "C0:A0": Entry
          val c   = "A0:B1": Entry
          val ab  = Seq[Entry]("A1:B0", "C0:A0")
          val ac  = Seq[Entry]("A1:B0,B1<-A0")
          val bc  = Seq[Entry]("C0:A0,B1<-A0")
          val abc = Seq[Entry]("A1:B0,B1<-A0", "C0:A0,B1<-A0")
          "ab" - assertAdd(a, b)(ab: _*)
          "ac" - assertAdd(a, c)(ac: _*)
          "bc" - assertAdd(b, c)(bc: _*)
          "abc" - assertConsolidation((ab :+ c): _*)(abc: _*)
          "acb" - assertConsolidation((ac :+ b): _*)(abc: _*)
        }
      }
    }
  }
}
