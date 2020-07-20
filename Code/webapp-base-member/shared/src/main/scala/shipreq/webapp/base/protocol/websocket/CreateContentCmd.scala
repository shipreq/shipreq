package shipreq.webapp.base.protocol.websocket

import shipreq.base.util.{Direction, NonEmptyArraySeq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text

/**
 * A command to create new content in a Project.
 */
sealed trait CreateContentCmd
object CreateContentCmd {

  def empty(reqTypeId: ReqTypeId): CreateContentCmd =
    reqTypeId match {
      case StaticReqType.UseCase => CreateUseCase.empty
      case rt: CustomReqTypeId   => CreateGenericReq.empty(rt)
    }

  final case class CreateGenericReq(codes     : Set[ReqCode.Value],
                                    customText: Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText],
                                    imps      : Direction.Values[Set[ReqId]],
                                    reqType   : CustomReqTypeId,
                                    tags      : Set[ApplicableTagId],
                                    title     : Text.GenericReqTitle.OptionalText) extends CreateContentCmd {

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.NonEmptyText): CreateGenericReq =
      copy(customText = customText.updated(f, t))

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.OptionalText): CreateGenericReq =
      NonEmptyArraySeq.maybe(t, this)(addCustomText(f, _))

    def addImps(d: Direction, add: Set[ReqId]): CreateGenericReq =
      copy(imps = imps.mod(d, add ++ _))

    def addTags(add: Set[ApplicableTagId]): CreateGenericReq =
      copy(tags = add ++ tags)
  }

  object CreateGenericReq {
    def empty(reqType: CustomReqTypeId): CreateGenericReq =
      apply(Set.empty, UnivEq.emptyMap, Direction.Values.both(Set.empty), reqType, Set.empty, ArraySeq.empty)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final case class CreateUseCase(codes     : Set[ReqCode.Value],
                                 customText: Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText],
                                 imps      : Direction.Values[Set[ReqId]],
                                 tags      : Set[ApplicableTagId],
                                 title     : Text.UseCaseTitle.OptionalText) extends CreateContentCmd {

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.NonEmptyText): CreateUseCase =
      copy(customText = customText.updated(f, t))

    def addCustomText(f: CustomField.Text.Id, t: Text.CustomTextField.OptionalText): CreateUseCase =
      NonEmptyArraySeq.maybe(t, this)(addCustomText(f, _))

    def addImps(d: Direction, add: Set[ReqId]): CreateUseCase =
      copy(imps = imps.mod(d, add ++ _))

    def addTags(add: Set[ApplicableTagId]): CreateUseCase =
      copy(tags = add ++ tags)
  }

  object CreateUseCase {
    def empty: CreateUseCase =
      apply(Set.empty, UnivEq.emptyMap, Direction.Values.both(Set.empty), Set.empty, ArraySeq.empty)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final case class CreateCodeGroup(code : ReqCode.Value,
                                   title: Text.CodeGroupTitle.OptionalText) extends CreateContentCmd


  // ===================================================================================================================
  object CodecsV3 {
    import boopickle.DefaultBasic._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._
    import shipreq.webapp.base.protocol.binary.v1.Rev5.AtomPicklers.instances._
    // REMEMBER: Don't forget to increment `CodecsVn` if you change these

    private implicit val picklerCreateGenericReq: Pickler[CreateGenericReq] =
      new Pickler[CreateGenericReq] {
        override def pickle(a: CreateGenericReq)(implicit state: PickleState): Unit = {
          state.pickle(a.codes)
          state.pickle(a.customText)
          state.pickle(a.imps)
          state.pickle(a.reqType)
          state.pickle(a.tags)
          state.pickle(a.title)
        }
        override def unpickle(implicit state: UnpickleState): CreateGenericReq = {
          val codes      = state.unpickle[Set[ReqCode.Value]]
          val customText = state.unpickle[Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText]]
          val imps       = state.unpickle[Direction.Values[Set[ReqId]]]
          val reqType    = state.unpickle[CustomReqTypeId]
          val tags       = state.unpickle[Set[ApplicableTagId]]
          val title      = state.unpickle[Text.GenericReqTitle.OptionalText]
          CreateGenericReq(codes, customText, imps, reqType, tags, title)
        }
      }

    private implicit val picklerCreateUseCase: Pickler[CreateUseCase] =
      new Pickler[CreateUseCase] {
        override def pickle(a: CreateUseCase)(implicit state: PickleState): Unit = {
          state.pickle(a.codes)
          state.pickle(a.customText)
          state.pickle(a.imps)
          state.pickle(a.tags)
          state.pickle(a.title)
        }
        override def unpickle(implicit state: UnpickleState): CreateUseCase = {
          val codes      = state.unpickle[Set[ReqCode.Value]]
          val customText = state.unpickle[Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText]]
          val imps       = state.unpickle[Direction.Values[Set[ReqId]]]
          val tags       = state.unpickle[Set[ApplicableTagId]]
          val title      = state.unpickle[Text.UseCaseTitle.OptionalText]
          CreateUseCase(codes, customText, imps, tags, title)
        }
      }

    private implicit val picklerCreateCodeGroup: Pickler[CreateCodeGroup] =
      new Pickler[CreateCodeGroup] {
        override def pickle(a: CreateCodeGroup)(implicit state: PickleState): Unit = {
          state.pickle(a.code)
          state.pickle(a.title)
        }
        override def unpickle(implicit state: UnpickleState): CreateCodeGroup = {
          val code  = state.unpickle[ReqCode.Value]
          val title = state.unpickle[Text.CodeGroupTitle.OptionalText]
          CreateCodeGroup(code, title)
        }
      }

    implicit val picklerCreateContentCmd: Pickler[CreateContentCmd] =
      new Pickler[CreateContentCmd] {
        private[this] final val KeyCreateCodeGroup  = 'c'
        private[this] final val KeyCreateGenericReq = 'g'
        private[this] final val KeyCreateUseCase    = 'u'
        override def pickle(a: CreateContentCmd)(implicit state: PickleState): Unit =
          a match {
            case b: CreateCodeGroup  => state.enc.writeByte(KeyCreateCodeGroup ); state.pickle(b)
            case b: CreateGenericReq => state.enc.writeByte(KeyCreateGenericReq); state.pickle(b)
            case b: CreateUseCase    => state.enc.writeByte(KeyCreateUseCase   ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): CreateContentCmd =
          state.dec.readByte match {
            case KeyCreateCodeGroup  => state.unpickle[CreateCodeGroup]
            case KeyCreateGenericReq => state.unpickle[CreateGenericReq]
            case KeyCreateUseCase    => state.unpickle[CreateUseCase]
          }
      }
  }
}
