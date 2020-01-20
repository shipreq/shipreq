package shipreq.webapp.base.protocol

import japgolly.univeq._

final case class Version(major: Version.Major, minor: Version.Minor) {
  override def toString = verStr
  def verNum = s"${major.value}.${minor.value}"
  def verStr = "v" + verNum
}

object Version {

  def fromInts(major: Int, minor: Int): Version =
    Version(Major(major), Minor(minor))

  final case class Major(value: Int) {
    assert(value >= 1)
  }

  final case class Minor(value: Int) {
    assert(value >= 0)
  }

  implicit def univEqMajor: UnivEq[Major]   = UnivEq.derive
  implicit def univEqMinor: UnivEq[Minor]   = UnivEq.derive
  implicit def univEq     : UnivEq[Version] = UnivEq.derive

  implicit val ordering: Ordering[Version] =
    new Ordering[Version] {
      override def compare(x: Version, y: Version): Int = {
        val i = x.major.value - y.major.value
        if (i != 0)
          i
        else
          x.minor.value - y.minor.value
      }
    }

  val v10 = fromInts(1, 0)
  val v11 = fromInts(1, 1)
}
