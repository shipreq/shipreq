package shipreq.base.util.diff

import japgolly.microlibs.stdlib_ext.StdlibExt._

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
    s"DiffSource($offset, $length)(${value.toString.quote})"
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

    type Split = DiffSource[DiffSource[String, Char], DiffSource[String, Char]]

    def split(input: String, splitOn: Char): Split =
      split(input, splitOn == _)

    def split(input: String, isSplit: Char => Boolean): Split =
      if (input.isEmpty)
        DiffSource.empty(DiffSource.empty(input))
      else {

        val lines = {
          val l = input.length
          val b = ArraySeq.newBuilder[DiffSource[String, Char]]

          @tailrec
          def go(offset: Int): Unit = {

            var i = offset
            val doSplit = isSplit(input.charAt(i))
            while ({
              i += 1
              i < l && isSplit(input.charAt(i)) == doSplit
            }) ()

            b += new StringSource(input, offset, i - offset)

            if (i < l)
              go(i)
          }

          go(0)

          b.result()
        }

        val root = Str(input)

        def newInstance(linesOffset: Int, linesLength: Int): Split =
          new Split {
            override def offset        = linesOffset
            override def length        = linesLength
            override def apply(i: Int) = lines(linesOffset + i)

            override protected def _slice(start: Int, newLen: Int) =
              newInstance(linesOffset + start, newLen)

            override def value =
              if (linesOffset == 0 && linesLength == lines.length)
                root
              else {
                val strStart    = if (linesOffset == 0) 0 else lines(linesOffset).offset
                val lineEndExcl = linesOffset + linesLength
                val strEndExcl  = if (lineEndExcl >= lines.length) input.length else lines(lineEndExcl).offset
                root.slice(strStart, strEndExcl)
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
      Auto(DiffSource.Str.split(_, '\n'))
  }

}
