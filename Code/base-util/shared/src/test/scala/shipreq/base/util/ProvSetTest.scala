package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import utest._
import nyaya.gen._
import nyaya.test.PropTest._

object ProvSetTest extends TestSuite {
  import ProvSet.Value
  import PartialOrder.Cmp._

  private object Internals {
    final case class K(id: String, rev: Int) {
      override def toString = s"$id$rev"
    }
    type V = String
    type M = String

    implicit def univEqK: UnivEq[K] = UnivEq.derive
    implicit val keyOrder = keyedInt((_: K).id, (_: K).rev)
    val module = ProvSet.Module[K, V, M](_.into)
    import module.{Entry, empty}

    implicit def autoValue(v: V): Value[V] = Value.Live(v)

    private val parseK: String => K = {
      val regex = "^([A-Za-z]+)(\\d+)$".r
      s => {
        val regex(id, rev) = s
        K(id, rev.toInt)
      }
    }

    def entry(id: String, prov: String = ""): Entry = {
      val k = parseK(id)
      val provs = prov.split(',').iterator.filter(_.nonEmpty).map(parseK).toSet
      module.entry(k, Value.Live(id), "M" + id, provs)
    }

    implicit def strToEntry(s: String): Entry =
      s.indexOf(':') match {
        case -1 => entry(s)
        case n  => entry(s.take(n), s.drop(n + 1))
      }

    def assertAdd(i: Entry, j: Entry)(expect: Entry*)(implicit l: Line): Unit = {
      val e =  expect.toSet
      assertEq(s"$i + $j", (empty + i + j).repr, expect = e)
      assertEq(s"$j + $i [reverse]", (empty + j + i).repr, expect = e)
    }

//    def assertConsolidation(inputs: Entry*)(expect: Entry*)(implicit l: Line): Unit = {
//      val actual = module.consolidate(inputs: _*).repr
////      assertSet(actual)(expect: _*)
//      assertEq(actual, expect = expect.toSet)
//    }
  }

  // ===================================================================================================================
  import Internals._

  override def tests = Tests {
    "manual" - {
      "basic" - {
        "gt"  - assertAdd("A2", "A3")("A3")
        "lt"  - assertAdd("A2", "A1")("A2")
        "eq"  - assertAdd("A2", "A2")("A2")
        "sep" - assertAdd("A2", "B3")("A2", "B3")
      }
      "prov" - {
//        "x"      - assertAdd("A0", "C2:A2")("C2:A2")
        "eq"     - assertAdd("A2:B3", "B3")("A2:B3")
        "gt"     - assertAdd("A2:B3", "B2")("A2:B3")
        "lt"     - assertAdd("A2:B3", "B4")("A2:B3", "B4")
        "sep"    - assertAdd("A2:B3", "C1:B2")("A2:B3", "C1:B2")
        "mergeA" - assertAdd("A2:B3", "B2:C6")("A2:B3,C6")
        "mergeL" - assertAdd("A2:B3,C8", "B2:C6")("A2:B3,C8")
        "mergeG" - assertAdd("A2:B3,C1", "B2:C4")("A2:B3,C4")
      }
    }

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
          live <- Gen.boolean
          k    <- genK
          prov <- genId.set(0 to size).map(_ - k.id).flatMap(Gen.traverse(_)(id => genRev.map(K(id, _))))
        } yield {
          val v = if (live) Value.Live(s"${k.id}=${k.rev}") else Value.Tombstone
          module.entry(k, v, s"M${k.id}${k.rev}", prov)
        }

      val genPS: Gen[ProvSet] =
        genE.list(0 to size).map(es => module.consolidate(es : _*))

      val gen: Gen[Input] =
        Gen.lift3(genPS, genPS, genPS)(ProvSet.Laws.Input(_, _, _))

//      import japgolly.microlibs.stdlib_ext.StdlibExt._
//      gen.withSeed(0).samples().take(100).drain()
      laws.mustBeSatisfiedBy(gen.withSeed(0))
    }
  }
}
