package shipreq.webapp.shared.data.delta

import upickle.{Reader, Writer}
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.{data => D}
import shipreq.webapp.shared.protocol.DataCodecs._

final case class Rev(value: Long) extends TaggedLong

sealed trait Partition {
  type Instance
  type Id

  implicit val rd: Reader[Id]
  implicit val wd: Writer[Id]
  implicit val rp: Reader[Instance]
  implicit val wp: Writer[Instance]

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

  sealed abstract class Aux[P, D](implicit RD: Reader[D], WD: Writer[D], RP: Reader[P], WP: Writer[P]) extends Partition {
    override type Instance = P
    override type Id = D
    override implicit val rd = RD
    override implicit val wd = WD
    override implicit val rp = RP
    override implicit val wp = WP
  }

  abstract class Fns[P <: Partition] {
    def rev(p: Project): Rev
    def update(p: Project, rev: Rev, ds: RemoteDeltaP[P]): Project
  }

  // ------------------------------------------------------------------------------------------------------------------

  case object CustReqType extends Aux[D.CustReqType, D.CustReqType.Id]
}

