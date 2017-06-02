package shipreq.webapp.base.protocol

import boopickle._
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.Direction
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text.Text
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecData._
import BinCodecEvents._
import AtomPicklers.instances._

/**
 * A command to create new content in a Project.
 */
sealed trait CreateContentCmd
object CreateContentCmd {

  final case class CreateGenericReq(codes     : Set[ReqCode.Value],
                                    customText: Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText],
                                    imps      : Direction.Values[Set[ReqId]],
                                    reqType   : CustomReqTypeId,
                                    tags      : Set[ApplicableTagId],
                                    title     : Text.GenericReqTitle.OptionalText) extends CreateContentCmd {

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.NonEmptyText): CreateGenericReq =
      copy(customText = customText.updated(f, t))

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.OptionalText): CreateGenericReq =
      NonEmptyVector.maybe(t, this)(addCustomText(f, _))

    def addImps(d: Direction, add: Set[ReqId]): CreateGenericReq =
      copy(imps = imps.mod(d, add ++ _))

    def addTags(add: Set[ApplicableTagId]): CreateGenericReq =
      copy(tags = add ++ tags)
  }

  object CreateGenericReq {
    def empty(reqType: CustomReqTypeId): CreateGenericReq =
      apply(Set.empty, UnivEq.emptyMap, Direction.Values.both(Set.empty), reqType, Set.empty, Vector.empty)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class CreateUseCase(codes     : Set[ReqCode.Value],
                                 customText: Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText],
                                 imps      : Direction.Values[Set[ReqId]],
                                 tags      : Set[ApplicableTagId],
                                 title     : Text.UseCaseTitle.OptionalText) extends CreateContentCmd {

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.NonEmptyText): CreateUseCase =
      copy(customText = customText.updated(f, t))

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.OptionalText): CreateUseCase =
      NonEmptyVector.maybe(t, this)(addCustomText(f, _))

    def addImps(d: Direction, add: Set[ReqId]): CreateUseCase =
      copy(imps = imps.mod(d, add ++ _))

    def addTags(add: Set[ApplicableTagId]): CreateUseCase =
      copy(tags = add ++ tags)
  }

  object CreateUseCase {
    def empty: CreateUseCase =
      apply(Set.empty, UnivEq.emptyMap, Direction.Values.both(Set.empty), Set.empty, Vector.empty)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class CreateCodeGroup(code : ReqCode.Value,
                                   title: Text.CodeGroupTitle.OptionalText) extends CreateContentCmd

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit val pickleCreateGenericReq: Pickler[CreateGenericReq] = pickleCaseClass
  implicit val pickleCreateUseCase   : Pickler[CreateUseCase   ] = pickleCaseClass
  implicit val pickleCreateCodeGroup : Pickler[CreateCodeGroup ] = pickleCaseClass
  implicit val pickleCmd             : Pickler[CreateContentCmd] = pickleADT
}

object CreateContentFn extends RemoteFn.ToVE[CreateContentCmd]
