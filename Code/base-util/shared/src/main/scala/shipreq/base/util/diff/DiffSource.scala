package shipreq.base.util.diff

abstract class DiffSource[+A] {
  def length: Int
  def apply(idx: Int): A

  def slice(from: Int, until: Int): DiffSource[A] = {
    val start = Math.max(from, 0)
    val end   = Math.min(until, length)
    val newLen = end - start
    if (newLen <= 0)
      DiffSource.Empty
    else if (from == 0 && newLen == length)
      this
    else
      _slice(start, newLen)
  }

  protected def _slice(start: Int, newLen: Int): DiffSource[A] =
    new DiffSource.Sub(this, start, newLen)

  @inline final def take     (n: Int) = slice(0, n)
  @inline final def drop     (n: Int) = slice(n, length)
  @inline final def takeRight(n: Int) = slice(length - Math.max(n, 0), length)
  @inline final def dropRight(n: Int) = slice(0, length - Math.max(n, 0))
}

object DiffSource {

  private[DiffSource] final class Sub[+A](real: DiffSource[A], offset: Int, val length: Int) extends DiffSource[A] {
    override def apply(idx: Int): A =
      real(offset + idx)

    override protected def _slice(start: Int, newLen: Int): DiffSource[A] =
      new Sub(real, offset + start, newLen)
  }

  object Empty extends DiffSource[Nothing] {
    override def length =
      0

    override def apply(idx: Int) =
      throw new IllegalArgumentException

    override def slice(from: Int, until: Int) =
      this
  }

  // ===================================================================================================================

  final case class Auto[-A, +E](wrap: A => DiffSource[E]) extends AnyVal

  object Auto {
    implicit def string: Auto[String, Char] =
      Auto(fromString)
  }

  def fromString(str: String): DiffSource[Char] =
    new DiffSource[Char] {
      override def length          = str.length
      override def apply(idx: Int) = str.charAt(idx)
    }
}
