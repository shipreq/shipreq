package shipreq.webapp.base.protocol

import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecBaseData._
import BinCodecMemberData._
import BinCodecUser._
import BinCodecEvents._

/**
  * Protocols for the Project SPA / webapp-client-project module.
  */
object ProjectSpaProtocols {

  final case class InitAsyncData(project: Project, projectMetaData: ProjectMetaData, latestEventOrd: EventOrd)
  implicit val picklerInitAsyncData = pickleCaseClass[InitAsyncData]

  val InitAsync             = ServerSideProc.Protocol[Unit, ErrorMsg \/ InitAsyncData]("Project.InitAsync")
  val ProjectNameSet        = ServerSideProc.Protocol.toEvents[String]("Project.ProjectNameSet")
  val FieldMandatorinessMod = ServerSideProc.Protocol.toEvents[(CustomFieldId, Mandatory)]("Project.FieldMandatorinessMod")
  val ReqTypeImplicationMod = ServerSideProc.Protocol.toEvents[(CustomReqTypeId, ImplicationRequired)]("Project.ReqTypeImplicationMod")
  val CreateContent         = ServerSideProc.Protocol.toEvents[CreateContentCmd]("Project.CreateContent")
  val UpdateContent         = ServerSideProc.Protocol.toEvents[UpdateContentCmd]("Project.UpdateContent")
  val UpdateSavedViews      = ServerSideProc.Protocol.toEvents[SavedViewCmd]("Project.SavedViewCmd")
  val CustomIssueTypeCrud   = CrudProtocol[CustomIssueTypeId, (HashRefKey, Option[String])]("Project.CustomIssueTypeCrud")
  val CustomReqTypeCrud     = CrudProtocol[CustomReqTypeId, (ReqType.Mnemonic, String, ImplicationRequired)]("Project.CustomReqTypeCrud")

  import InitAsync            .{pickleInstance => _i1}
  import ProjectNameSet       .{pickleInstance => _i2}
  import CustomIssueTypeCrud  .{pickleInstance => _i3}
  import CustomReqTypeCrud    .{pickleInstance => _i4}
  import FieldMandatorinessMod.{pickleInstance => _i5}
  import ReqTypeImplicationMod.{pickleInstance => _i6}
  import CreateContent        .{pickleInstance => _i7}
  import UpdateContent        .{pickleInstance => _i8}
  import UpdateSavedViews     .{pickleInstance => _i9}
  import TagCrud.Protocol     .{pickleInstance => _i10}
  import FieldCrud.Protocol   .{pickleInstance => _i11}

  final case class InitData(username        : Username,
                            projectName     : Project.Name,
                            initAsync       : InitAsync            .Instance,
                            issueTypeCrud   : CustomIssueTypeCrud  .Instance,
                            reqTypeCrud     : CustomReqTypeCrud    .Instance,
                            reqTypeImpMod   : ReqTypeImplicationMod.Instance,
                            fieldMandMod    : FieldMandatorinessMod.Instance,
                            fieldCrud       : FieldCrud.Protocol   .Instance,
                            tagCrud         : TagCrud.Protocol     .Instance,
                            createContent   : CreateContent        .Instance,
                            updateContent   : UpdateContent        .Instance,
                            updateSavedViews: UpdateSavedViews     .Instance,
                            projectNameSet  : ProjectNameSet       .Instance)
  implicit val picklerInitData = pickleCaseClass[InitData]

  final val EntryPointName = "P"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)

  final val CometListenerName = "C"
  val CometListener = ClientSideProc[VerifiedEvent.NonEmptySeq](CometListenerName)
}