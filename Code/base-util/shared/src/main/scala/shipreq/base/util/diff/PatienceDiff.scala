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

        var m: Match = null

        if (slice.abNonEmpty)
          m = patienceSort(unique(a, b, slice))

        if (m == null) {

          if (slice.aNonEmpty | slice.bNonEmpty) {
            f(slice)
          }

        } else {
          var aIdx = slice.aStart
          var bIdx = slice.bStart
          var aNext = 0
          var bNext = 0

          while(true) {
            if (m == null) {
              aNext = slice.aEndExcl
              bNext = slice.bEndExcl
            } else {
              aNext = m.aIdx
              bNext = m.bIdx
            }

            val subSlice = new Slice(aIdx, aNext, bIdx, bNext)
            matchHead(subSlice)
            matchTail(subSlice)
            go(subSlice)

            if (m == null)
              return

            aIdx = m.aIdx
            bIdx = m.bIdx
            m = m.next
          }
        }

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

  } // Internals
}

final class PatienceDiff[A: UnivEq](fallback: DiffAlgorithm[A]) extends DiffAlgorithm[A] {
  import PatienceDiff.Internals._

  override def writeDiff(a : DiffSource[A],
                         b : DiffSource[A],
                         pw: PatchWriter): Unit = {

    val pw2 = new PatchWriter.WithMutableOffsets(pw)

    patienceDiff(a, b) { slice =>
      val a2 = a.slice(slice.aStart, slice.aEndExcl)
      val b2 = b.slice(slice.bStart, slice.bEndExcl)
      pw2.srcOffset = slice.aStart
      pw2.tgtOffset = slice.bStart
      fallback.writeDiff(a2, b2, pw2)
    }
  }

}
