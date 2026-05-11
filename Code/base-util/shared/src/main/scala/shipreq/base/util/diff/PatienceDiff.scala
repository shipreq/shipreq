package shipreq.base.util.diff

object PatienceDiff {

  def apply[A: UnivEq](fallback: DiffAlgorithm[Any, A]) =
    new PatienceDiff[
      Any, // S
      A,   // A
      A,   // B
      Any, // T
      A,   // C
    ](
      lineValue  = identity,
      mergeLines = identity,
      fallback   = fallback,
    )

  def twoDims[S: UnivEq, C](fallback: DiffAlgorithm[Any, C]) =
    new PatienceDiff[
      DiffSource[S, C], // S
      DiffSource[S, C], // A
      S,                // B
      Any,              // T
      C,                // C
    ](
      lineValue  = _.value,
      mergeLines = _.value,
      fallback   = fallback,
    )

  def splitStrings(fallback: DiffAlgorithm[Any, Char]) =
    twoDims[String, Char](fallback)

  // ===================================================================================================================

  private object Internals {

    final class Slice(_aStart  : Int,
                      _aEndExcl: Int,
                      _bStart  : Int,
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

    def unique[A, B: UnivEq](x: DiffSource[Any, A],
                             y: DiffSource[Any, A],
                             slice: Slice)
                            (f: A => B): Array[Match] = {

      val counts = collection.mutable.LinkedHashMap.empty[B, UniqueCounter]

      for (i <- slice.aRange) {
        val b = f(x(i))
        val c = counts.getOrElseUpdate(b, new UniqueCounter)
        c.aCount += 1
        if (c.aFirstIdx < 0)
          c.aFirstIdx = i
      }

      for (i <- slice.bRange) {
        val b = f(y(i))
        val c = counts.getOrElseUpdate(b, new UniqueCounter)
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

    def patienceDiff[A, B: UnivEq](a: DiffSource[Any, A],
                                   b: DiffSource[Any, A])
                                  (lineValue: A => B)
                                  (f: Slice => Unit): Unit = {

      val slice = new Slice(0, a.length, 0, b.length)

      def go(): Unit = {

        var m: Match = null

        if (slice.abNonEmpty)
          m = patienceSort(unique(a, b, slice)(lineValue))

        if (m == null) {

          if (slice.aNonEmpty | slice.bNonEmpty) {
            f(slice)
          }

        } else {
          val aLast = slice.aEndExcl // save before mutation
          val bLast = slice.bEndExcl // save before mutation
          var aIdx = slice.aStart
          var bIdx = slice.bStart
          var aNext = 0
          var bNext = 0

          while(true) {

            if (m == null) {
              aNext = aLast
              bNext = bLast
            } else {
              aNext = m.aIdx
              bNext = m.bIdx
            }

            // Prepare slice for recursive call
            slice.aStart = aIdx
            slice.bStart = bIdx
            slice.aEndExcl = aNext
            slice.bEndExcl = bNext

            // matchHead
            while (slice.abNonEmpty && a(slice.aStart) == b(slice.bStart)) {
              slice.aStart += 1
              slice.bStart += 1
            }

            // matchTail
            while (slice.abNonEmpty && a(slice.aEndExcl - 1) == b(slice.bEndExcl - 1)) {
              slice.aEndExcl -= 1
              slice.bEndExcl -= 1
            }

            // Recurse
            go()

            if (m == null)
              return

            aIdx = m.aIdx + 1
            bIdx = m.bIdx + 1
            m = m.next
          }
        }
      }

      go()
    }

  } // Internals
}

/**
 * @param lineValue Converts an input DiffSource element to a line value
 * @param mergeLines Convert a slice of the input into a new source for processing by the fallback diff algorithm
 * @param fallback The fallback diff algorithm to use for mismatched blocks (once lines are aligned)
 * @tparam S Input DiffSource values
 * @tparam A Input DiffSource elements
 * @tparam B Representation of line values that has UnivEq
 * @tparam T Line diff algorithm values
 * @tparam C Line diff algorithm elements
 */
final class PatienceDiff[S, A, B: UnivEq, T, C](lineValue : A => B,
                                                mergeLines: DiffSource[S, A] => DiffSource[T, C],
                                                fallback  : DiffAlgorithm[T, C]) extends DiffAlgorithm[S, A] {
  import PatienceDiff.Internals._

  @inline def widen: DiffAlgorithm[S, A] =
    this

  override def writeDiff(a : DiffSource[S, A],
                         b : DiffSource[S, A],
                         pw: PatchWriter): Unit = {

    val pw2 = new PatchWriter.WithMutableOffsets(pw)

    patienceDiff(a, b)(lineValue) { slice =>
      val aLines = a.slice(slice.aStart, slice.aEndExcl)
      val bLines = b.slice(slice.bStart, slice.bEndExcl)

      val aSubStr = mergeLines(aLines)
      val bSubStr = mergeLines(bLines)

      pw2.srcOffset = aSubStr.offset
      pw2.tgtOffset = bSubStr.offset

      fallback.writeDiff(aSubStr, bSubStr, pw2)
    }
  }

}
