package shipreq.webapp.base.protocol

import scalaz.\&/
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.RemoteDelta
import Routine._

import upickle.TupleCodecs._
import DataCodecs._
import ProtocolDataCodecs._
import DeltaCodecs._

object Routines {

  object ProjectInit extends DescT[Unit, Project]

  object CustomIssueTypeCrud extends Crudable.CAux[CustomIssueType.Id, CustomIssueTypeProtocol.Values]
  object CustomReqTypeCrud   extends Crudable.CAux[CustomReqType.Id,   CustomReqTypeProtocol.Values]
  object TagCrud             extends Crudable.CAux[Tag.Id,             TagProtocol.Values \&/ TagProtocol.PovRelations]

  object CustomReqTypeImplicationMod extends DescT[(CustomReqType.Id, ImplicationRequired), RemoteDelta]

  object FieldCrud extends DescT[FieldProtocol.CfgAction, RemoteDelta]

  case class ProjectSPA(projectInit:   ProjectInit                .Remote,
                        issueTypeCrud: CustomIssueTypeCrud        .Remote,
                        reqTypeCrud:   CustomReqTypeCrud          .Remote,
                        reqTypeImpMod: CustomReqTypeImplicationMod.Remote,
                        tagCrud:       TagCrud                    .Remote)
}