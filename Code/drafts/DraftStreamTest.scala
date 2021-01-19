package shipreq.base.test.drafts

import sourcecode.Line
import shipreq.base.test.BaseTestUtil._
import utest.{assert => _, _}
import shipreq.base.util.NonEmptyArraySeq
import shipreq.base.util.diff._
import shipreq.webapp.member.jsfacade.Yjs

object DraftStreamTest extends TestSuite {
  import Yjs2._
  import Yjs.{Doc, YText}
  import DraftStream._

//  private def debug(str: String = ""): Unit =
//    println(str)
//
//  final class Dsl(baseOrd: Int) {
//    private var prev = Option.empty[StateVector]
//    private var doc = new Doc
//    private var rows = Vector.empty[Row]
//
//    def this(ds: DraftStream) = {
//      this(ds.baseOrd)
//      rows = ds.rows.whole
//      doc = ds.doc
//      prev = Some(doc.toStateVector)
//    }
//
//    def init(e: Event): this.type =
//      init(e.ord, e.value)
//
//    def init(evOrd: Int, value: String = null): this.type = {
//      val v = if (value == null) s"[event $evOrd]" else value
//      val e = Event(evOrd, v)
//      doc = Client.initDoc(Some(e))
//      this
//    }
//
//    def modText(f: YText => Unit): this.type = {
//      f(doc.getText())
//      this
//    }
//
//    def append(s: String)                      = modText(_.append(s))
//    def prepend(s: String)                     = modText(_.prepend(s))
//    def insert(index: Int, content: String)    = modText(_.insert(index, content))
//    def delete(index: Int, length: Int)        = modText(_.delete(index, length))
//    def deleteLast(length: Int)                = modText(_.deleteLast(length))
//    def replaceFirst(from: String, to: String) = modText(_.replaceFirst(from, to))
//
//    def writeRow(evOrd: Int, dirty: Boolean): this.type = {
//      val delta = doc.toUpdateSV(prev)
//      val row = Row(delta, evOrd, dirty)
//      rows :+= row
//      prev = Some(doc.toStateVector)
//      this
//    }
//
//    def result(): DraftStream = {
//      assert(rows.nonEmpty, "No rows added")
//      DraftStream(baseOrd, NonEmptyVector.force(rows))
//    }
//  }
//
//  def newStream(baseOrd: Int)(f: Dsl => Dsl): DraftStream =
//    f(new Dsl(baseOrd)).result()
//
//  def modStream(i: DraftStream)(f: Dsl => Dsl): DraftStream =
//    f(new Dsl(i)).result()
//
////  def assertDraftStream(ds: DraftStream)
////                       (expectEvOrd: Int, expectDirty: Boolean, expectText: String)(implicit l: Line): Unit = {
////    val expect = (expectEvOrd, expectDirty, expectText)
////    val actual = (ds.lastEventOrd, ds.dirty, ds.text)
////    assertEq(actual, expect)
////  }
////
////  def assertDraftStream(desc: => String, ds: DraftStream)
////                       (expectEvOrd: Int, expectDirty: Boolean, expectText: String)(implicit l: Line): Unit = {
////    val expect = (expectEvOrd, expectDirty, expectText)
////    val actual = (ds.lastEventOrd, ds.dirty, ds.text)
////    assertEq(desc, actual, expect)
////  }
////
////  def merge(from: DraftStream, to: DraftStream, events: Seq[Event], lastQty: Int = 0): (DraftStream, DraftStream) = {
////    val fromSend = if (lastQty != 0) lastQty else from.rows.length
////    val fromSkip = from.rows.length - fromSend
////    val sendRows = NonEmptyVector force from.rows.whole.drop(fromSkip)
////    debug()
////    debug(s"sending @$fromSkip $sendRows")
////
////    val toStore = new MutableStore
////    toStore.init(to)
////    toStore.add(from.baseOrd, fromSkip, sendRows) match {
////
////      case \/-(()) =>
////        val to2 = toStore.get().get
////        (from, to2)
////
////      case -\/(Some(newBase)) =>
////        val from2 = from.rebase(newBase, events)
////        val skip = newBase.rows.length
////        val sendRows2 = NonEmptyVector force from2.rows.whole.drop(skip)
////        debug("rebased: " + from2.text)
////        debug(s"re-sending @$skip $sendRows2")
////        val r = toStore.add(from2.baseOrd, skip, sendRows2)
////        utest.assert(r.isRight)
////        val to2 = toStore.get().get
////        (from2, to2)
////
////      case -\/(None) =>
////        fail("Unsupported: -\\/(None)")
////    }
////  }
////
////  def assertMerge(from: DraftStream, to: DraftStream, events: Seq[Event], lastQty: Int = 0)
////                 (expectEvOrd: Int, expectDirty: Boolean, expectText: String)(implicit l: Line): Unit = {
////    val (from2, to2) = merge(from, to, events, lastQty)
////    assertEq(if (from eq from2) "from.text" else "from2.text (rebase failed)", from2.text, expectText)
////    assertDraftStream("from2", from2)(expectEvOrd, expectDirty, expectText)
////    assertDraftStream("to2", to2)(expectEvOrd, expectDirty, expectText)
////  }
////
////  private val diffAlgo = PatienceDiff.splitStrings(MyersLinearDiff[Char])
////
////  def applyDiff(before: String, after: String, t: YText): Unit = {
////    val modFn = diffAlgo.diff(before, after)(YjsPatchFactory)
////    modFn(t)
////  }
//
//  val e3 = Event(3, "[event 3]")
//  val e4 = Event(4, "[event 4]")
//  val e5 = Event(5, "[event 5]")
//  val es3 = Seq(e3)
//  val es4 = Seq(e3, e4)
//  val es5 = Seq(e3, e4, e5)
//
//  val initDelta3 = newUpdate(3)(_.getText().append(e3.value))
//  val initDelta5 = newUpdate(5)(_.getText().append(e5.value))

  private def rows[A](rowValues: A*): NonEmptyArraySeq[Row[A]] =
    NonEmptyArraySeq.force(rowValues.iterator.map(Row(_)).to(ArraySeq))

  private def newStream[A](rowValues: A*): DraftStream[A] =
    DraftStream(Id.random(), rows(rowValues: _*))

  override def tests = Tests {

    "add" - {

      "follows" - {
        val a = newStream(4, 3)
        val b = Partial(a.steamId, 2, rows(7, 8))
        val e = a.copy(rows = rows(4, 3, 7, 8))
        assertEq(a.add(b), Some(e))
      }

      "diffId" - {
        val a = newStream(4, 3)
        val b = Partial(Id.random(), 2, rows(7, 8))
        assertEq(a.add(b), None)
      }

      "over1" - {
        val a = newStream(4, 3)
        val b = Partial(a.steamId, 3, rows(7, 8))
        assertEq(a.add(b), None)
      }

      "under1ok" - {
        val a = newStream(4, 3)
        val b = Partial(a.steamId, 1, rows(3, 8))
        val e = a.copy(rows = rows(4, 3, 8))
        assertEq(a.add(b), Some(e))
      }

      "under1ko" - {
        val a = newStream(4, 3)
        val b = Partial(a.steamId, 1, rows(7, 8))
        assertEq(a.add(b), None)
      }

      "redundant" - {
        val a = newStream(4, 3)
        val b = Partial(a.steamId, 1, rows(3))
        assertEq(a.add(b), Some(a))
      }
    }

//    "sameBase" - {
//
//      // same base, same start & end events - two edits
//      "ee" - {
//        val a = newStream(3)(_.init(e3).append(" >>").writeRow(3, true))
//        val b = newStream(3)(_.init(e3).prepend("<< ").writeRow(3, true))
//        def test(from: DraftStream, to: DraftStream): Unit =
//          assertMerge(from, to, es3, 1)(3, true, "<< [event 3] >>")
//        "ab" - test(a, b)
//        "ba" - test(b, a)
//      }
//
////      // same base, same start & end events - edit + abort
////      "ea" - {
////        val x = newStream(3)(_.init(e3).prepend("<< ").writeRow(3, true))
////        val a = modStream(x)(_.delete(0, 3).writeRow(3, false)) // abort
////        val b = modStream(x)(_.append(" !!").writeRow(3, true))
////        def test(from: DraftStream, to: DraftStream): Unit =
////          assertMerge(from, to, es3, 1)(3, true, "[event 3] !!")
////        "ab" - test(a, b)
////        "ba" - test(b, a)
////      }
////
////      // same base, same start & end events - abort + abort
////      "aa" - {
////        val x = newStream(3)(_.init(e3).prepend("<< ").writeRow(3, true))
////        val a = modStream(x)(_.delete(0, 3).writeRow(3, false)) // abort
////        val b = modStream(x)(_.append(" !!").delete(0, 3).writeRow(3, false).deleteLast(3).writeRow(3, false)) // abort
////        def test(from: DraftStream, to: DraftStream): Unit =
////          assertMerge(from, to, es3, 1)(3, false, "[event 3]")
////        "ab" - test(a, b)
////        "ba" - test(b, a)
////      }
////
////      // same base, same start events - commit + edit
////      "ce" - {
////        val a = newStream(3)(_.init(e3).replaceFirst("3", "4!").writeRow(4, false))
////        val b = newStream(3)(_.init(e3).append(" +").writeRow(3, true))
////        val es = Seq(e3, Event(4, "[event 4!]"))
////        def test(from: DraftStream, to: DraftStream): Unit =
////          assertMerge(from, to, es)(4, true, "[event 4!] +")
////        "ab" - test(a, b)
////        "ba" - test(b, a)
////      }
////
////      // same base, same start events - commit + edit
////      "ca" - {
////        val a = newStream(3)(_.init(e3).replaceFirst("3", "4!").writeRow(4, false))
////        val b = newStream(3)(_.init(e3).prepend("!").writeRow(3, true).delete(0, 1).writeRow(3, false))
////        val es = Seq(e3, Event(4, "[event 4!]"))
////        def test(from: DraftStream, to: DraftStream): Unit =
////          assertMerge(from, to, es)(4, false, "[event 4!]")
////        "ab" - test(a, b)
////        "ba" - test(b, a)
////      }
//    }
//
//    "diffBase" - {
//
////      "ee" - {
////        val a = newStream(3)(_.init(e3).append(" >>").writeRow(3, true))
////        val b = newStream(5)(_.init(e5).prepend("<< ").writeRow(5, true))
////        def test(from: DraftStream, to: DraftStream): Unit =
////          assertMerge(from, to, es5)(5, true, "<< [event 5] >>")
//////        "ab" - test(a, b)
//////        "ba" - test(b, a)
////
////        "rebase 5 on 3" - {
////          // add [4,5] & 5.1 to 3c
////
////          val delta35 = {
////            var tmp = new Doc
////            tmp.clientID = 5
////            tmp += initDelta3
////            val sv = tmp.toStateVector
////            applyDiff(e3.value, e5.value, tmp.getText()) // apply diff between e3 & e5
////            tmp.toUpdate(sv)
////          }
////
////          val delta_51_on_35 = {
////            val tmp = new Doc
////            tmp += initDelta3
////            tmp += delta35
////            val sv = tmp.toStateVector
////            applyDiff(e5.value, b.text, tmp.getText()) // apply diff between e5 & dirty5
////            assertEq(tmp.getText().strValue(), "<< [event 5]")
////            tmp.toUpdate(sv)
////          }
////
////          val newDelta = {
////            val tmp = new Doc
////            tmp += initDelta3
////            val sv = tmp.toStateVector
////            tmp += delta35
////            tmp += delta_51_on_35
////            tmp.toUpdate(sv)
////          }
////
////          val tmp = a.doc
////          tmp += newDelta
////          assertEq(tmp.getText().strValue(), "<< [event 5] >>")
////        }
////
////        "rebase 3 on 5" - {
////          val sv3clean = StateVector(a.doc.getMap("svs").get("3").asInstanceOf[Yjs.StateVector])
////          val dirty3 = a.doc.toUpdate(sv3clean)
////
////          val delta35 = {
////            var tmp = new Doc
////            tmp.clientID = 5
////            tmp += initDelta3
////            val sv = tmp.toStateVector
////            applyDiff(e3.value, e5.value, tmp.getText()) // apply diff between e3 & e5
////            tmp.toUpdate(sv)
////          }
////
////          var delta_31_on_35_text = ""
////          val delta_31_on_35 = {
////            val tmp = new Doc
////            tmp += initDelta3
////            tmp += delta35
////            val sv = tmp.toStateVector
////            tmp += dirty3
////            assertEq(tmp.getText().strValue(), "[event 5] >>")
////            delta_31_on_35_text = tmp.getText().strValue()
////            tmp.toUpdate(sv)
////          }
////
////          val delta_31_on_5 = {
////            val tmp = new Doc
////            tmp += initDelta5
////            val sv = tmp.toStateVector
////            locally(delta_31_on_35)
////            applyDiff(e5.value, delta_31_on_35_text, tmp.getText()) // apply diff between e5 & delta_31_on_35
////            assertEq(tmp.getText().strValue(), "[event 5] >>")
////            tmp.toUpdate(sv)
////          }
////
////          // delta_31_on_5 is the new event rebased on 5
////          val tmp = b.doc
////          tmp += delta_31_on_5
////          assertEq(tmp.getText().strValue(), "<< [event 5] >>")
////
//////          val clean3 = new Doc
//////          clean3 += initDelta3
////
//////          val clean5 = new Doc
//////          clean5 += newDelta(5)(_.getText().append(e5.value))
//////
//////          val `d3.1 against clean3` = a.doc.toUpdate(clean3)
//////          val `d5.1 against clean5` = b.doc.toUpdate(clean5)
//////
//////          val d = new Doc
////////          d += initDelta3
//////          d.getText().applyDelta(clean3.getText().toDelta())
//////          console.log("[0a] ", d.getText().strValue())
//////          console.log("[0b] ", d.getText().toDelta())
//////          val s = d.snapshot
//////          //d += `d3.1 against clean3`
//////          val delta31 =
//////          d.getText().applyDelta(clean3.getText().toDelta())
//////          val t = d.snapshot
//////          console.log("[1a] ", d.getText().strValue())
//////          console.log("[1b] ", d.getText().toDelta())
//////          console.log("[1c] ", d.getText().toDelta(s))
//////          console.log("[1d] ", d.getText().toDelta(s, s))
//////          console.log("[1e] ", d.getText().toDelta(t, s))
//////          console.log("[1f] ", d.getText().toDelta(s, t))
//////          console.log("[1g] ", d.getText().toDelta(t))
//////
//////          println("="*100)
////
//////          ;{
//////            val d = new Doc
//////            val t = d.getText()
//////            d += newDelta(0)(_.getText().append("this is the start"))
//////            val sv = d.toStateVector
//////            t.replaceFirst("start", "end")
//////            val delta = d.toUpdate(sv)
//////
//////            val d2 = new Doc
//////            val t2 = d2.getText()
//////            d2 += newDelta(0)(_.getText().append("this is the start"))
//////            d2 += delta
//////            t2.strValue()
//////          }
////
//////          ;{
//////            val d = new Doc
//////            val t = d.getText()
//////            d += newDelta(0)(_.getText().append("this is the start"))
//////            val ss = d.snapshot
//////            t.replaceFirst("start", "end")
//////            val ss2 = d.snapshot
//////            console.log("[2-] ", t.toDelta())
//////            console.log("[2a] ", t.toDelta(ss))
//////            console.log("[2b] ", t.toDelta(ss2))
//////            console.log("[2c] ", t.toDelta(ss, ss2))
//////            console.log("[2d] ", t.toDelta(ss2, ss))
//////            console.log("[2e] ", t.toDelta(ss, ss))
//////            console.log("[2f] ", t.toDelta(ss2, ss2))
//////
//////            val d2 = new Doc
//////            val t2 = d2.getText()
//////            d2 += newDelta(0)(_.getText().append("this is the start"))
//////            t2.applyDelta(t.toDelta(ss2, ss))
//////            t2.strValue()
//////          }
////
////        }
//
////      }
//    }
//
  }
}
