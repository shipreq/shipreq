package shipreq.webapp.base.data

import japgolly.nyaya._
import monocle.macros.GenLens
import scalaz.Equal
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.base.util.UnivEq
import shipreq.webapp.base.util.TypeclassDerivation._

// =====================================================================================================================

/** A monotonic revision number. */
case class Rev(value: Long) extends TaggedLong {
  @inline def succ      = Rev(value + 1L)
  @inline def +(r: Rev) = Rev(value + r.value)
}

// =====================================================================================================================

case class RevRange(fromInclusive: Rev, toInclusive: Rev) {
  this assertSatisfies RevRange.prop

  override def toString = s"[${fromInclusive.value}-${toInclusive.value}]"

  def contains(rev: Rev): Boolean =
    (rev >= fromInclusive) && (rev <= toInclusive)
}

object RevRange {
  implicit def equality: UnivEq[RevRange] = deriveUnivEq

  def single(rev: Rev) = RevRange(rev, rev)

  lazy val prop =
    Prop.test[RevRange]("from ≥ 0", _.fromInclusive >= 0) ∧
    Prop.test[RevRange]("from ≤ to", r => r.fromInclusive <= r.toInclusive)
}

// =====================================================================================================================

final case class RevAnd[D](rev: Rev, data: D)

trait RevAndLowPri {
  implicit def equality[D](implicit r: UnivEq[Rev], d: Equal[D]): Equal[RevAnd[D]] =
    Equal.equal((a, b) => (a.rev == b.rev) && d.equal(a.data, b.data))
}

object RevAnd extends RevAndLowPri {
  implicit def universalEquality[D](implicit r: UnivEq[Rev], d: UnivEq[D]): UnivEq[RevAnd[D]] = UnivEq.force

  def rev [D] = GenLens[RevAnd[D]](_.rev)
  def data[D] = GenLens[RevAnd[D]](_.data)
}
