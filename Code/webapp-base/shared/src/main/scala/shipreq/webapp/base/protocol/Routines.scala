package shipreq.webapp.base.protocol

import scalaz.\&/
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.RemoteDelta
import Routine._

import BinCodecGeneric._
import BinCodecData._
import BinCodecProtocolData._
import BinCodecDelta._

object Routines {
  // After adding a new Routine, also update the following:
  // - ProtocolRemoteCodecs
  // - RandomData


  object ProjectInit extends (Unit =>|=> Project)

  // Project config
  object CustomIssueTypeCrud   extends Crudable.CAux[CustomIssueTypeId, CustomIssueTypeProtocol.Values]
  object CustomReqTypeCrud     extends Crudable.CAux[CustomReqTypeId,   CustomReqTypeProtocol.Values]
  object TagCrud               extends Crudable.CAux[TagId,             TagProtocol.Values \&/ TagProtocol.PovRelations]
  object FieldCrud             extends (FieldProtocol.CfgAction                =>|=> RemoteDelta)
  object FieldMandatorinessMod extends ((CustomFieldId,   Mandatory          ) =>|=> RemoteDelta)
  object ReqTypeImplicationMod extends ((CustomReqTypeId, ImplicationRequired) =>|=> RemoteDelta)

  object UpdateProjectContent extends (ContentUpdate =>|=> RemoteDelta)


  case class ProjectSPA(projectInit:   ProjectInit          .Remote,
                        issueTypeCrud: CustomIssueTypeCrud  .Remote,
                        reqTypeCrud:   CustomReqTypeCrud    .Remote,
                        reqTypeImpMod: ReqTypeImplicationMod.Remote,
                        fieldMandMod:  FieldMandatorinessMod.Remote,
                        fieldCrud:     FieldCrud            .Remote,
                        tagCrud:       TagCrud              .Remote,
                        updateContent: UpdateProjectContent .Remote)
}