package shipreq.webapp.base.protocol

import scalaz.\&/
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.VerifiedEvents
import RemoteFn._

import BinCodecGeneric._
import BinCodecData._
import BinCodecProtocolData._
import BinCodecEvents._

object RemoteFns {
  // After adding a new RemoteFn, also update the following:
  // - RandomData
  // - ProtocolTest

  object ProjectInit extends (Unit =>|=> Project)

  // Project config
  object CustomIssueTypeCrud   extends Crudable.CAux[CustomIssueTypeId, (HashRefKey, Option[String])]
  object CustomReqTypeCrud     extends Crudable.CAux[CustomReqTypeId,   (ReqType.Mnemonic, String, ImplicationRequired)]
  object TagCrud               extends Crudable.CAux[TagId,             TagProtocol.Values \&/ TagInTree.Relations]
  object FieldCrud             extends (FieldProtocol.CfgAction                =>|=> VerifiedEvents)
  object FieldMandatorinessMod extends ((CustomFieldId,   Mandatory          ) =>|=> VerifiedEvents)
  object ReqTypeImplicationMod extends ((CustomReqTypeId, ImplicationRequired) =>|=> VerifiedEvents)

  object UpdateProjectContent extends (ContentUpdate =>|=> VerifiedEvents)


  case class ProjectSPA(projectInit:   ProjectInit          .Instance,
                        issueTypeCrud: CustomIssueTypeCrud  .Instance,
                        reqTypeCrud:   CustomReqTypeCrud    .Instance,
                        reqTypeImpMod: ReqTypeImplicationMod.Instance,
                        fieldMandMod:  FieldMandatorinessMod.Instance,
                        fieldCrud:     FieldCrud            .Instance,
                        tagCrud:       TagCrud              .Instance,
                        updateContent: UpdateProjectContent .Instance)
}