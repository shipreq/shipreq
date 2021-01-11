package shipreq.base.util.diff

object PatienceDiff {

  def apply[A: UnivEq](fallback: DiffAlgorithm[A]): PatienceDiff[A] =
    new PatienceDiff(fallback)

  private object Internals {

    final class Slice(_aStart: Int,
                      _aEndExcl: Int,
                      _bStart: Int,
                      _bEndExcl: Int,
                     ) {
      var aStart   = _aStart
      var aEndExcl = _aEndExcl
      var bStart   = _bStart
      var bEndExcl = _bEndExcl

      @inline def aRange    = aStart until aEndExcl
      @inline def bRange    = bStart until bEndExcl
      @inline def aNonEmpty = aStart < aEndExcl
      @inline def bNonEmpty = bStart < bEndExcl
      @inline def aIsEmpty  = !aNonEmpty
      @inline def bIsEmpty  = !bNonEmpty
      def abNonEmpty        = aNonEmpty && bNonEmpty

      @elidable(elidable.FINEST)
      override def toString = s"[$aStart,$aEndExcl)+[$bStart,$bEndExcl)"
    }

    final class UniqueCounter {
      var aCount = 0
      var bCount = 0
      var aFirstIdx = -1
      var bFirstIdx = -1
    }

    final class Match(val aIdx: Int, val bIdx: Int) {
      var prev: Match = null
      var next: Match = null

      @elidable(elidable.FINEST)
      override def toString = s"($aIdx,$bIdx)${Option(next).fold("")(" -> " + _)}"
    }

    def unique[A: UnivEq](a: DiffSource[A],
                          b: DiffSource[A],
                          slice: Slice): Array[Match] = {

      val counts = collection.mutable.LinkedHashMap.empty[A, UniqueCounter]

      for (i <- slice.aRange) {
        val t = a(i)
        val c = counts.getOrElseUpdate(t, new UniqueCounter)
        c.aCount += 1
        if (c.aFirstIdx < 0)
          c.aFirstIdx = i
      }

      for (i <- slice.bRange) {
        val t = b(i)
        val c = counts.getOrElseUpdate(t, new UniqueCounter)
        c.bCount += 1
        if (c.bFirstIdx < 0)
          c.bFirstIdx = i
      }

      counts
        .valuesIterator
        .filter(c => c.aCount == 1 && c.bCount == 1)
        .map(c => new Match(c.aFirstIdx, c.bFirstIdx))
        .toArray
    }

    def patienceSort(matches: Array[Match]): Match =
      if (matches.isEmpty)
        null
      else {

        val stacks = collection.mutable.ArrayBuffer.empty[Match]
        var lo, hi, mid = 0

        // phase 1
        for (m <- matches) {

          // binary search
          lo = -1
          hi = stacks.length
          while (lo + 1 < hi) {
            mid = (lo + hi) >> 1
            if (stacks(mid).bIdx < m.bIdx)
              lo = mid
            else
              hi = mid
          }

          // update data
          if (lo >= 0)
            m.prev = stacks(lo)
          lo += 1
          if (lo == stacks.length)
            stacks.addOne(m)
          else
            stacks.update(lo, m)
        }

        // phase 2
        var m = stacks.last
        while (m.prev != null) {
          m.prev.next = m
          m = m.prev
        }

        m
      }

    def patienceDiff[A: UnivEq](a: DiffSource[A],
                                b: DiffSource[A])
                               (f: Slice => Unit): Unit = {

      @inline def matchHead(slice: Slice): Unit =
        while (slice.abNonEmpty && a(slice.aStart) == b(slice.bStart)) {
          slice.aStart += 1
          slice.bStart += 1
        }

      @inline def matchTail(slice: Slice): Unit =
        while (slice.abNonEmpty && a(slice.aEndExcl - 1) == b(slice.bEndExcl - 1)) {
          slice.aEndExcl -= 1
          slice.bEndExcl -= 1
        }

      def go(slice: Slice): Unit = {
        println(s">  GO : $slice")

        // TODO cache unique
        var m: Match = null
        if (slice.abNonEmpty)
          m = patienceSort(unique(a, b, slice))

        println(s"MATCH : $m")
        if (m == null) {

          if (slice.aNonEmpty | slice.bNonEmpty) {
            println(s" DIFF : $slice")
            f(slice)
          }

        } else {
          var aIdx = slice.aStart
          var bIdx = slice.bStart
          var aNext = 0
          var bNext = 0
          while ( {
            if (m == null) {
              aNext = slice.aEndExcl
              bNext = slice.bEndExcl
            } else {
              aNext = m.aIdx
              bNext = m.bIdx
            }

            // TODO Opt - reuse slice? or reuse var
            val subSlice = new Slice(aIdx, aNext, bIdx, bNext)
            matchHead(subSlice)
            matchTail(subSlice)
            go(subSlice)
            println(s"<  GO")

            if (m == null) {
              false
            } else {
              aIdx = m.aIdx
              bIdx = m.bIdx
              m = m.next
              true
            }
          }) ()
        }

        /*
        ==============================================================================================
        line[12] = [ab]Idx
        end[12]  = slice.[ab]EndExcl - 1
        next[12] = [ab]Next / m.[ab]Idx
        ==============================================================================================

        var aIdx = slice.aStart
        var bIdx = slice.bStart
        var aNext = 0
        var bNext = 0
        while ({

          if (m == null) {
            aNext = slice.aEndExcl - 1
            bNext = slice.bEndExcl - 1
          } else {
            aNext = m.aIdx
            bNext = m.bIdx

            while (aNext > aIdx && bNext > bIdx && a(aNext - 1) == b(bNext - 1)) {
              aNext -= 1
              bNext -= 1
            }
          }

          while (aIdx < aNext && bIdx < bNext && a(aIdx) == b(bIdx)) {
            aIdx += 1
            bIdx += 1
          }

          if (aNext > aIdx || bNext > bIdx) {
            val subSlice = new Slice(aIdx, aNext, bIdx, bNext)
            go(subSlice)
            println(s"<  GO")
          }

          if (m == null) {
            false

          } else {

            while (
              m.next != null &&
                m.next.aIdx == m.aIdx + 1 &&
                m.next.bIdx == m.bIdx + 1
            )
              m = m.next

            aIdx = m.aIdx + 1
            bIdx = m.bIdx + 1
            m = m.next
            true
          }

        }) ()

//        var end1 = slice.aEndExcl
//        var end2 = slice.bEndExcl
//        var next1, next2 = 0
//
//        var first: Match = null
//        if (slice.abNonEmpty)
//          first = patienceSort(unique(a, b, slice))
//
//        while ({
//
//          if (first != null) {
//            next1 = first.aIdx
//            next2 = first.bIdx
//            val subSlice = new Slice(aIdx, next1, bIdx, next2)
//            matchTail(s)
//
//          } else {
//
//          }
//
//
//          ???
//        }) ()
*/
      }

      go(new Slice(0, a.length, 0, b.length))
    }

//    def patienceDiff2[S: UnivEq, C](a: DiffSource[S],
//                                    b: DiffSource[S],
//                                    c: S => DiffSource[C],
//                                    f: DiffAlgorithm[C]): Unit = {
//      patienceDiff(a, b) { s =>
//        // a.slice(s.aStart, s.aEndExcl).flatMap(c)
//        // or maybe better would be: String => DiffSource[DiffSource[Char]] + UnivEq[DiffSource]
//        // or maybe better would be: DiffSource[A] => DiffSource[DiffSource[A]] + UnivEq[DiffSource]
//      }
//    }

    def patienceDiff_Git[A: UnivEq](a: DiffSource[A],
                                    b: DiffSource[A],
                                    p: PatchWriter
                                   )
                                   (f: Slice => Unit): Unit = {

      var walk_common_sequence: (Match, Int, Int, Int, Int) => Unit =
        null

      def patience_diff(line1: Int, count1: Int,
                        line2: Int, count2: Int,
                       ): Unit = {

        if (count1 == 0) {
          if (count2 != 0) {
            // nothing on left, lines on right
            p.insert(
              srcIdx = line1,
              tgtIdx = line2,
              length = count2
            )
          }
        } else if (count2 == 0) {
          // lines on left, nothing on right
          p.delete(
            srcIdx = line1,
            length = count1
          )
        } else
        {
          var s = new Slice(line1, line1 + count1, line2, line2 + count2)
          var m: Match = patienceSort(unique(a, b, s))
          if (m == null) {
            f(s)
          } else {
            walk_common_sequence(m, line1, count1, line2, count2)
          }
        }

      }

      walk_common_sequence = (_first, _line1, count1, _line2, count2) => {
        var line1 = _line1
        var line2 = _line2
        val end1 = line1 + count1
        val end2 = line2 + count2
        var next1,next2 = 0
        var first = _first

        while ({

          if (first != null) {
            next1 = first.aIdx
            next2 = first.bIdx
            while (next1 > line1 && next2 > line2 && a(next1 - 1) == b(next2 - 1)) {
              next1 -= 1
              next2 -= 1
            }
          } else {
            next1 = end1
            next2 = end2
          }

          while (line1 < next1 && line2 < next2 && a(line1) == b(line2)) {
            line1 += 1
            line2 += 1
          }

          if (next1 > line1 || next2 > line2) {
            patience_diff(
              line1, next1 - line1,
              line2, next2 - line2,
            )
            // break?
          }

          if (first == null) {
            false
          } else {

            while (
              first.next != null
              && first.next.aIdx == first.aIdx + 1
              && first.next.bIdx == first.bIdx + 1
            ) {
              first = first.next
            }

            line1 = first.aIdx + 1
            line2 = first.bIdx + 1
            first = first.next

            true
          }

        }) ()
      }

      patience_diff(0, a.length, 0, b.length)
    }

  } // Internals
}

final class PatienceDiff[A: UnivEq](fallback: DiffAlgorithm[A]) extends DiffAlgorithm[A] {
  import PatienceDiff.Internals._

  override def writeDiff(a : DiffSource[A],
                         b : DiffSource[A],
                         pw: PatchWriter): Unit = {

    val pw2 = new PatchWriter.WithMutableOffsets(pw)

//    patienceDiff_Git(a, b, pw) { slice =>
    patienceDiff(a, b) { slice =>
      val a2 = a.slice(slice.aStart, slice.aEndExcl)
      val b2 = b.slice(slice.bStart, slice.bEndExcl)
      pw2.srcOffset = slice.aStart
      pw2.tgtOffset = slice.bStart
      fallback.writeDiff(a2, b2, pw2)
    }
  }

}
