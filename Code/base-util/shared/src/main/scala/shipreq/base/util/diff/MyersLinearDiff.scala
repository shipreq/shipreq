package shipreq.base.util.diff

/** Myers diff in linear space,
  * as described in "An O(ND) Difference Algorithm and Its Variations" by Eugene W. Myers (1986).
  *
  * Time = O(nd)
  * Space = O(n)
  * where
  *   n = original.length min revised.length
  *   d = number of differences
  *
  * https://blog.robertelder.org/diff-algorithm
  * https://docs.rs/crate/diffs/0.4.0/source/src/myers.rs
  */
object MyersLinearDiff extends DiffAlgorithm {

  override def writeDiff[A](original  : DiffSource[A],
                            revised   : DiffSource[A],
                            patch     : PatchWriter)
                           (implicit A: DiffEqual[A]): Unit = {

    import DiffSource.{Empty => emptyView}

    val g = new Array[Int]((Math.min(original.length, revised.length) + 1) << 1)
    val p = new Array[Int](g.length)

    def go(e: DiffSource[A],
           f: DiffSource[A],
           i: Int,
           j: Int
          ): Unit = {
      val N = e.length
      val M = f.length
      if (N > 0 && M > 0) {
        val L = N + M
        val Z = (Math.min(N, M) + 1) << 1
        val w = N - M
        java.util.Arrays.fill(g, 0)
        java.util.Arrays.fill(p, 0)
        val hLast = (L >> 1) + mod2(L)
        var c = g
        var d = g
        var o = 0
        var m = 0
        var k = 0
        var h = 0
        while (h <= hLast) {
          val kLast = h - (Math.max(0, h - N) << 1)
          c = g
          d = p
          o = 1
          m = 1
          while (o >= 0) {
            k = -(h - (Math.max(0, h - M) << 1))
            while (k <= kLast) {
              var a =
                if (k == -h || (k != h && c(mod(k - 1, Z)) < c(mod(k + 1, Z))))
                  c(mod(k + 1, Z))
                else
                  c(mod(k - 1, Z)) + 1
              var b = a - k
              val s = a
              val t = b
              while (
                a < N
                  && b < M
                  && A.eql(e((1 - o) * N + m * a + (o - 1)), f((1 - o) * M + m * b + (o - 1)))
              ) {
                a += 1
                b += 1
              }
              c(mod(k, Z)) = a
              val z = -(k - w)
              if (mod2(L) == o && z >= -(h - o) && z <= h - o && c(mod(k, Z)) + d(mod(z, Z)) >= N) {
                var D = 0
                var x = 0
                var y = 0
                var u = 0
                var v = 0
                if (o == 0) {
                  D = h << 1
                  x = N - a
                  y = M - b
                  u = N - s
                  v = M - t
                } else {
                  D = (h << 1) - 1
                  x = s
                  y = t
                  u = a
                  v = b
                }
                if (D > 1 || (x != u && y != v)) {
                  go(e.take(x), f.take(y), i, j)
                  go(e.slice(u, N), f.slice(v, M), i + u, j + v)
                } else if (M > N)
                  go(emptyView, f.slice(N, M), i + N, j + N)
                else if (M < N)
                  go(e.slice(M, N), emptyView, i + M, j + M)
                return
              }
              k += 2
            }
            o -= 1
            if (o == 0) {
              c = p
              d = g
              m = -1
            }
          }
          h += 1
        }
        assert(false) // impossible

      } else if (N > 0)
        patch.delete(i, N)
      else if (M > 0)
        patch.insert(i, j, M)
    }

    go(original, revised, 0, 0)
  }

  @inline
  private def mod(a: Int, b: Int): Int =
    ((a % b) + b) % b
  //    {
  //      val m = a % b
  //      if (m >= 0) m else m + b
  //    }

  @inline
  private def mod2(a: Int): Int =
    a & 1

}
