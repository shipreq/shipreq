package shipreq.webapp.base.protocol

import scalaz.\&/
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.RemoteDelta
import Routine._

import upickle.TupleCodecs._
import GenericCodecs._
import DataCodecs._
import ProtocolDataCodecs._
import DeltaCodecs._

object Routines {

  object ProjectInit extends DescT[Unit, Project]

  object CustomIssueTypeCrud extends Crudable.CAux[CustomIssueType.Id, CustomIssueTypeProtocol.Values]
  object CustomReqTypeCrud   extends Crudable.CAux[CustomReqTypeId,    CustomReqTypeProtocol.Values]
  object TagCrud             extends Crudable.CAux[TagId,              TagProtocol.Values \&/ TagProtocol.PovRelations]

  object FieldMandatorinessMod extends DescT[(CustomField.Id , Mandatory          ), RemoteDelta]
  object ReqTypeImplicationMod extends DescT[(CustomReqTypeId, ImplicationRequired), RemoteDelta]

  object FieldCrud extends DescT[FieldProtocol.CfgAction, RemoteDelta]

  case class ProjectSPA(projectInit:   ProjectInit          .Remote,
                        issueTypeCrud: CustomIssueTypeCrud  .Remote,
                        reqTypeCrud:   CustomReqTypeCrud    .Remote,
                        reqTypeImpMod: ReqTypeImplicationMod.Remote,
                        fieldMandMod:  FieldMandatorinessMod.Remote,
                        fieldCrud:     FieldCrud            .Remote,
                        tagCrud:       TagCrud              .Remote)
}