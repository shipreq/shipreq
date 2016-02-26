package shipreq.webapp.base.protocol

import shipreq.base.util.UnivEq, UnivEq.Implicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import boopickle._, BoopickleMacros._, BinCodecGeneric._, BinCodecData._, BinCodecEvents._, AtomPicklers.instances._
import shipreq.webapp.base.text.Text

/**
 * A command to create new content in a Project.
 */
sealed trait CreateContentCmd
object CreateContentCmd {

  case class CreateGenericReq(rt      : CustomReqTypeId,
                              title   : Text.GenericReqTitle.OptionalText,
                              reqCodes: Set[ReqCode.Value],
                              tags    : Set[ApplicableTagId],
                              impSrcs : Set[ReqId]) extends CreateContentCmd

  case class CreateUseCase(title   : Text.UseCaseTitle.OptionalText,
                           reqCodes: Set[ReqCode.Value],
                           tags    : Set[ApplicableTagId],
                           impSrcs : Set[ReqId]) extends CreateContentCmd

  case class CreateReqCodeGroup(code : ReqCode.Value,
                                title: Text.ReqCodeGroupTitle.OptionalText) extends CreateContentCmd

  implicit val pickleCreateGenericReq  : Pickler[CreateGenericReq  ] = pickleCaseClass
  implicit val pickleCreateUseCase     : Pickler[CreateUseCase     ] = pickleCaseClass
  implicit val pickleCreateReqCodeGroup: Pickler[CreateReqCodeGroup] = pickleCaseClass
  implicit val pickleCmd               : Pickler[CreateContentCmd  ] = pickleADT
}

object CreateContentFn extends RemoteFn.ToVE[CreateContentCmd]
