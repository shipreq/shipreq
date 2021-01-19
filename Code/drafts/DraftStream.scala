//package shipreq.base.test.drafts
//
//import shipreq.base.test.BaseTestUtil._
//import shipreq.webapp.member.jsfacade.Yjs
//import Yjs2._
//import DraftStream._
//
//object DraftStream {
//  final case class Row(delta: Update, eventOrd: Int, dirty: Boolean)
//
//  final case class Entry(baseOrd: Int, rowNo: Int, row: Row)
//
//  final class MutableStore {
//    private var state = Option.empty[DraftStream]
//
//    def get() = state
//
//    def init(s: DraftStream): Unit =
//      state = Some(s)
//
//    def add(baseOrd: Int, firstRowNo: Int, rows: NonEmptyVector[Row]): Option[DraftStream] \/ Unit = {
//      val follows =
//        state match {
//          case Some(s) => firstRowNo == s.rows.length && s.baseOrd == baseOrd
//          case None    => firstRowNo == 0
//        }
//      if (follows) {
//        val s2 = state match {
//          case Some(s) => s.copy(rows = s.rows ++ rows)
//          case None    => DraftStream(baseOrd, rows)
//        }
//        state = Some(s2)
//        \/-(())
//      } else
//        -\/(state)
//    }
//  }
//}
//
//final case class DraftStream(baseOrd: Int, rows: NonEmptyVector[Row]) {
//
//  private def debug(str: String): Unit =
//    println(str)
//
//  def firstEventOrd = rows.head.eventOrd
//  def lastEventOrd = rows.last.eventOrd
//  def dirty = rows.last.dirty
//
////  def addDelta(e: Entry): Option[DraftStream] =
////    ???
//
////  def rebase(b: DraftStream, events: Seq[Event]): DraftStream = {
////    def needEvent(ord: Int) = events.find(_.ord == ord).getOrThrow(s"Event $ord not found")
////    if (baseOrd != b.baseOrd) {
////
////      debug("---------------------------------------------------------------------------")
////      debug(s"rebase '$text' onto '${b.text}'")
////
////      //    clean common root
////      //
////      // base dirt       this dirt
////      //
////      //         merged
////
////      // Step 1. Upgrade base to latest ord
//////      val newLatestClean = lastEventOrd max b.lastEventOrd
//////      if (b.lastEventOrd < newLatestClean) {
//////        val d = new Yjs.Doc
//////        val latestClean =
//////        d += newDelta(ord)(_.getText().append(needEvent(ord).value))
//////      }
////
/////*
////      val (lo, hi) = if (b.firstEventOrd < this.firstEventOrd) (b, this) else (this, b)
////
////      debug(s"${lo.firstEventOrd}-${lo.lastEventOrd} + ${hi.firstEventOrd}-${hi.lastEventOrd}")
////      // i3
////      // d3.1
////      // +
////      // i5
////      // d5.1
////      // =
////      // i3
////      // d3-4
////      // d4-5
////      // d3.1
////      // (d5.1 - i5)
////
//////      var rows = lo.rows
////      var ord = lo.firstEventOrd
////      val last = lo.lastEventOrd max hi.lastEventOrd
////      val doc = new Yjs.Doc
////      debug(s"Starting with $ord")
////      doc += newDelta(ord)(_.getText().append(needEvent(ord).value))
////      while (ord < last) {
////        ord += 1
////        debug(s"Populating $ord")
////        doc += newDelta(ord) { _ =>
////          val t = doc.getText()
////          val from = needEvent(ord - 1).value
////          val to = needEvent(ord).value
////          genAndApplyDiff(t, from, to)
////        }
////      }
////
////      def calcChanges(ds: DraftStream): Update = {
////        val i = newDelta(ds.firstEventOrd)(_.getText().append(needEvent(ds.firstEventOrd).value))
////        val d = new Yjs.Doc
////        d += i
////        val u = ds.doc.toUpdate(d)
////        val x = d.getText().strValue()
////        d += u
////        println(s"  [${ds.doc.getText().strValue()}] : [$x] + changes = [${d.getText().strValue()}]")
////        u
////      }
////
////      val loChanges = calcChanges(lo)
////      val hiChanges = calcChanges(hi)
////
////      debug("Changes [0]: " + doc.getText().strValue())
////      doc += loChanges
////      debug("Changes [1]: " + doc.getText().strValue())
////      doc += hiChanges
////      debug("Changes [2]: " + doc.getText().strValue())
////      */
////
////      throw new RuntimeException("not yet supported...")
////
////    } else {
////      val doc = b.doc
////      debug(s"Rebase start: ${doc.getText().strValue()}")
////      val before = doc.toStateVector
////      debug(s"Rebasing ${rows.length} rows")
////      for (r <- rows) {
////        doc += r.delta
////        debug(s"  Rebased to ${doc.getText().strValue()}")
////      }
////      val eventOrd2 = lastEventOrd max b.lastEventOrd
////      val event = events.find(_.ord == eventOrd2).getOrThrow(s"Event $eventOrd2 not found")
////      val delta2 = doc.toUpdate(before)
////      val dirty2 = event.value != doc.getText().strValue()
////      val newRows = b.rows :+ Row(delta2, eventOrd2, dirty2)
////      b.copy(rows = newRows)
////    }
////  }
//
//  def doc = {
//    val d = new Yjs.Doc
//    for (r <- rows)
//      d += r.delta
//    d
//  }
//
//  def text: String =
//    doc.getText().strValue()
//}
