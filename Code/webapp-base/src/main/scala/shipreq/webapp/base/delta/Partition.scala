package shipreq.webapp.base.delta

import upickle.{Reader, Writer}
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.protocol.DataCodecs._
import shipreq.webapp.base.protocol.ProtocolDataCodecs._

sealed trait Partition {
  type Data
  type Id

  implicit val di: DataIdAux[Data, Id]
  implicit val ui: UnivEq[Id]
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

  // TODO rename/remove. Use ≟.
  def testEq[A <: Partition, B <: Partition](a: A, b: B): Option[EqProof[A, B]] =
    if (a eq b)
      Some(new EqProof[A, B])
    else
      None

  type Aux[D, I] = Partition {type Data = D; type Id = I}

  sealed abstract class AuxC[D, I](DI: DataIdAux[D, I])(implicit UI: UnivEq[I],
                                                        RI: Reader[I], WI: Writer[I],
                                                        RD: Reader[D], WD: Writer[D]) extends Partition {
    override final type Data = D
    override final type Id = I
    override final implicit val di = DI
    override final implicit val ui = UI
    override final implicit val ri = RI
    override final implicit val wi = WI
    override final implicit val rd = RD
    override final implicit val wd = WD
  }

  sealed abstract class AuxO[O, D, I](o: O)(implicit DI: ObjDataId[O, D, I], UI: UnivEq[I],
                                            RI: Reader[I], WI: Writer[I],
                                            RD: Reader[D], WD: Writer[D]) extends AuxC[D, I](DI)

  abstract class Fns[P <: Partition] {
    def rev(p: Project): Rev
    def update(p: Project, rev: Rev, ds: RemoteDeltaP[P]): Project
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Partition instances

  import shipreq.webapp.base.protocol._

  case object CustomIssueTypes extends AuxO(CustomIssueType)
  case object CustomReqTypes   extends AuxO(CustomReqType)
  case object Fields           extends AuxC(FieldProtocol.Delta)
  case object Tags             extends AuxO(TagProtocol.PovTag)

  val values = NonEmptyVector[Partition](
    CustomIssueTypes,
    CustomReqTypes,
    Fields,
    Tags)

  // All instaces are objects
  @inline implicit def equality = UnivEq.force[Partition]
}
