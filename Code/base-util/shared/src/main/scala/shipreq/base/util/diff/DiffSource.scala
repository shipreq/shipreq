package shipreq.base.util.diff

abstract class DiffSource[+S, +A] {
  assert(length >= 0 && offset >= 0, s"offset=$offset, length=$length")


  def offset: Int
  def length: Int
  def apply(idx: Int): A

  def value: S

  def empty(offset: Int): DiffSource[S, A]

  def slice(from: Int, until: Int): DiffSource[S, A] = {
    val start = Math.max(from, 0)
    val end   = Math.min(until, length)
    val newLen = end - start
    if (newLen <= 0)
      empty(start)
    else if (start == 0 && newLen == length)
      this
    else
      _slice(start, newLen)
  }

  protected def _slice(start: Int, newLen: Int): DiffSource[S, A]

  @inline final def endExclusive      = offset + length
  @inline final def take     (n: Int) = slice(0, n)
  @inline final def drop     (n: Int) = slice(n, length)
  @inline final def takeRight(n: Int) = slice(length - Math.max(n, 0), length)
  @inline final def dropRight(n: Int) = slice(0, length - Math.max(n, 0))
  @inline final def just     (n: Int) = slice(n, n + 1)

  @elidable(elidable.FINEST)
  override def toString =
    s"DiffSource($offset, $length): ${value.toString.quote}"
}

object DiffSource {

  def empty[S](s: S, offset: Int = 0): DiffSource[S, Nothing] = {
    val o = offset
    new DiffSource[S, Nothing] {
      override def value                            = s
      override def offset                           = o
      override def length                           = 0
      override def apply(i: Int)                    = throw new IllegalArgumentException()
      override def empty(offset: Int)               = DiffSource.empty(s, offset)
      override protected def _slice(a: Int, b: Int) = this
    }
  }

  // ===================================================================================================================

  object Str {

    def apply(str: String): DiffSource[String, Char] =
      new StringSource(str, 0, str.length)

    private final class StringSource(val rootValue: String,
                                     val offset   : Int,
                                     val length   : Int) extends DiffSource[String, Char] {

      override def value =
        rootValue.substring(offset, offset + length)

      override def apply(idx: Int) =
        rootValue.charAt(offset + idx)

      override def empty(offset: Int) =
        DiffSource.empty("", offset)

      override protected def _slice(start: Int, newLen: Int) =
        new StringSource(rootValue, offset + start, newLen)
    }

    type Lines = DiffSource[DiffSource[String, Char], DiffSource[String, Char]]

    def lines(input: String): Lines =
      if (input.isEmpty)
        DiffSource.empty(DiffSource.empty(input))
      else {

        val lines = {
          val b = ArraySeq.newBuilder[DiffSource[String, Char]]

          @tailrec
          def go(remainder: String, offset: Int): Unit = {
            var n = remainder.indexOf('\n')
            if (n >= 0) {
//              if (n != 0) b += new StringSource(input, offset, n)
                b += new StringSource(input, offset, n + 1)

              n += 1
              go(remainder.drop(n), offset + n)
            } else if (remainder.nonEmpty)
              b += new StringSource(input, offset, remainder.length)
          }

          go(input, 0)

          b.result()
        }

        val root = Str(input)

        def newInstance(linesOffset: Int, linesLength: Int): Lines =
          new Lines {
            override def offset        = linesOffset
            override def length        = linesLength
            override def apply(i: Int) = lines(linesOffset + i)

            override protected def _slice(start: Int, newLen: Int) =
              newInstance(linesOffset + start, newLen)

            override def value = {
              val v =
              if (linesOffset == 0 && linesLength == lines.length)
                root
              else {
                val strStart    = if (linesOffset == 0) 0 else lines(linesOffset).offset
                val lineEndExcl = linesOffset + linesLength
                val strEndExcl  = if (lineEndExcl >= lines.length) input.length else lines(lineEndExcl).offset
                root.slice(strStart, strEndExcl)
              }
//            println(s"----- .value(${input.quote}, $linesOffset, $linesLength) = ${v}")
            v
            }

            override def empty(o: Int) = {
              val strOffset =
                if (o < linesLength)
                  lines(o).offset
                else if (linesLength > 0)
                  lines(linesOffset + linesLength - 1).endExclusive
                else
                  0
              DiffSource.empty(DiffSource.empty("", strOffset), o)
            }
          }

        newInstance(0, lines.length)
      }
  }

  // ===================================================================================================================

  final case class Auto[-I, +S, +A](wrap: I => DiffSource[S, A]) extends AnyVal

  object Auto {
    implicit def string: Auto[String, String, Char] =
      Auto(DiffSource.Str.apply)

    implicit def stringLines: Auto[String, DiffSource[String, Char], DiffSource[String, Char]] =
      Auto(DiffSource.Str.lines)
  }

}
