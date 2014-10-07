package shipreq.webapp.shared.data.delta

import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.{data => D}

final case class Rev(value: Long) extends TaggedLong

sealed trait Partition {
  type Instance
  type Id

  final def unapply[B <: Partition](b: B): Option[Partition.EqProof[B, this.type]] =
    Partition.testEq[B, this.type](b, this)
}

case object Partition {

  final class EqProof[A <: Partition, B <: Partition] private[Partition]() {
    def subst[F[_ <: Partition]](a: F[A]) = a.asInstanceOf[F[B]]
  }

  def testEq[A <: Partition, B <: Partition](a: A, b: B): Option[EqProof[A, B]] =
    if (a eq b)
      Some(new EqProof[A, B])
    else
      None

    def forceEqProof[A <: Partition, B <: Partition]: EqProof[A, B] =
      new EqProof[A, B]

  // ------------------------------------------------------------------------------------------------------------------

  sealed abstract class Aux[P, D] extends Partition {
    override type Instance = P
    override type Id = D
  }

  abstract class Fns[P <: Partition] {
    def rev(p: Project): Rev
    def update(p: Project, rev: Rev, ds: RemoteDeltaP[P]): Project
  }

  // ------------------------------------------------------------------------------------------------------------------

  case object CustReqType extends Aux[D.CustReqType, D.CustReqType.Id]
}

