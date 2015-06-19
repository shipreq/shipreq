package shipreq.webapp.base.protocol

import shipreq.base.util.SetDiff
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text

/**
 * Protocol for editing a project.
 *
 * Note: The project itself is never explicitly specified here.
 */
object ProjectProtocol {

}

/*
Send ProjectRev alongside cmd?

This could be useful to send back to client.
Same code could be used on server & client to patch projects.

Or are there changes (knock-on effects) that require new instructions that would be server→client only?

 */

sealed trait ProjectChange
object ProjectChange {

  case class PatchReqTags        (id: ReqId,        patch: SetDiff[ApplicableTagId]) extends ProjectChange
  case class PatchImplicationSrc (id: ReqId,        patch: SetDiff[ReqId])           extends ProjectChange
  case class PatchImplicationTgt (id: ReqId,        patch: SetDiff[ReqId])           extends ProjectChange
  case class PatchReqCodes       (id: ReqId,        patch: SetDiff[ReqCode.Value])   extends ProjectChange

  case class SetGenericReqType   (id: GenericReqId, value: CustomReqTypeId) extends ProjectChange
  case class SetReqCodeGroupCode (id: ReqCodeId,    code: ReqCode.Value)    extends ProjectChange

  case class SetReqCodeGroupTitle(id: ReqCodeId,                              value: Text.ReqCodeGroupTitle.OptionalText) extends ProjectChange
  case class SetGenericReqTitle  (id: GenericReqId,                           value: Text.GenericReqTitle.OptionalText)   extends ProjectChange
  case class SetCustomTextField  (id: ReqId,        fid: CustomField.Text.Id, value: Text.CustomTextField.OptionalText)   extends ProjectChange

}