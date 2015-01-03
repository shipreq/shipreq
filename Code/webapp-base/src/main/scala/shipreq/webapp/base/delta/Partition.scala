package shipreq.webapp.base.delta

import scalaz.NonEmptyList
import upickle.{Reader, Writer}
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.protocol.TagProtocol
import shipreq.webapp.base.protocol.DataCodecs._

sealed trait Partition {
  type Data
  type Id

  implicit val di: DataIdAux[Data, Id]
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

  // ------------------------------------------------------------------------------------------------------------------

  type Aux[D, I] = Partition {type Data = D; type Id = I}

  sealed abstract class CAux[T, D, I](t: T)(implicit I: ObjDataId[T, D, I],
                                           RI: Reader[I], WI: Writer[I],
                                           RD: Reader[D], WD: Writer[D]) extends Partition {
    override type Data = D
    override type Id = I
    override implicit val di = I
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

  case object CustomIssueTypes extends CAux(CustomIssueType)
  case object CustomReqTypes   extends CAux(CustomReqType)
  case object Tags             extends CAux(TagProtocol.PovTag)

  val values = NonEmptyList[Partition](CustomIssueTypes, CustomReqTypes, Tags)
}
