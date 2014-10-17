package shipreq.webapp.shared.data.delta

import scalaz.NonEmptyList
import upickle.{Reader, Writer}
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.protocol.DataCodecs._

sealed trait Partition {
  type DI <: DataAndId
  final type Data = DI#Data
  final type Id = DI#Id

  implicit val ri: Reader[Id]
  implicit val wi: Writer[Id]
  implicit val rd: Reader[Data]
  implicit val wd: Writer[Data]

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

  sealed abstract class Aux[T <: DataAndId](implicit RI: Reader[T#Id], WI: Writer[T#Id], RD: Reader[T#Data], WD: Writer[T#Data]) extends Partition {
    override type DI = T
    override implicit val ri = RI
    override implicit val wi = WI
    override implicit val rd = RD
    override implicit val wd = WD
  }

  abstract class Fns[P <: Partition] {
    def rev(p: Project): Rev
    def update(p: Project, rev: Rev, ds: RemoteDeltaP[P]): Project
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Partition instances

  val values = NonEmptyList[Partition](CustomIncmpTypes, CustomReqTypes)

  case object CustomIncmpTypes extends Aux[CustomIncmpTypeAndId]
  case object CustomReqTypes   extends Aux[CustomReqTypeAndId]
}

