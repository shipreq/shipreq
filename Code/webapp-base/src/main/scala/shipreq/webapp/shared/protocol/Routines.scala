package shipreq.webapp.shared.protocol

import upickle.{Reader, Writer}
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta.RemoteDelta
import Codec._, DataCodecs._, DeltaCodecs._
import Routine._

sealed abstract class GenericCrud[T: Reader : Writer, I: Reader : Writer, V: Reader : Writer] {
  final type R = Option[T]
  final type Id = I
  final type Values = V

  case object Create extends DescT[V, R]
  case object Update extends DescT[(I, V), R]
  case object SoftDelete extends DescT[I, RemoteDelta]
  case object HardDelete extends DescT[I, R]
  case object Restore extends DescT[I, R]

  implicit def rr1 = remoteRoutine(Create)
  implicit def rr2 = remoteRoutine(Update)
  implicit def rr3 = remoteRoutine(SoftDelete)
  implicit def rr4 = remoteRoutine(HardDelete)
  implicit def rr5 = remoteRoutine(Restore)
}

object Routines {

  object CustReqTypeOps extends GenericCrud[CustReqType, CustReqType.Id, (ReqType.Mnemonic, String, ImplicationRequired)]

  case class ForCfgReqType(create: CustReqTypeOps.Create.Remote,
                           update: CustReqTypeOps.Update.Remote,
                           softDelete: CustReqTypeOps.SoftDelete.Remote,
                           hardDelete: CustReqTypeOps.HardDelete.Remote,
                           restore: CustReqTypeOps.Restore.Remote)
    extends Group
}
