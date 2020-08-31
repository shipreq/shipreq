package shipreq.base.util

object Aligner {

  def forStrings(): Mutable[String] =
    new Mutable(_.length)

  final class Mutable[I](f: I => Int) {
    private[this] var maxLen = 0

    def consider(i: I, plus: Int = 0): Unit = {
      val len = f(i) + plus
      if (len > maxLen)
        maxLen = len
    }

    def paddingSize(len: Int): Int =
      if (len >= maxLen) 0 else maxLen - len

    def padLeft(s: String): String = {
      var p = paddingSize(s.length)
      if (p == 0)
        s
      else {
        val sb = new StringBuilder(maxLen)
        sb.append(s)
        while (p > 0) {
          p -= 1
          sb append ' '
        }
        sb.toString
      }
    }

    def padLeft(sb: StringBuilder, s: String): Unit = {
      var p = paddingSize(s.length)
      sb.append(s)
      while (p > 0) {
        p -= 1
        sb append ' '
      }
    }

  }
}
