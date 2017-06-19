package shipreq.webapp.base.protocol

import shipreq.webapp.base.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.base.data._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecData._
import BinCodecEvents._

/**
  * Protocols for the Project SPA / webapp-client-project module.
  */
object ProjectSpaProtocols {

  // TODO The purpose of InitAsync was so the page (without the Project) could load fast and responsively,
  // and then the Project could be loaded, built remotely and sent over next.
  // Now with the `ProjectServer` logic, the project is loaded immediately. Will have to think about that... [TODO]
  final case class InitAsyncData(project: Project, latestEventOrd: EventOrd)
  implicit val picklerInitAsyncData = pickleCaseClass[InitAsyncData]

  val InitAsync             = ServerSideProc.Protocol[Unit, InitAsyncData]
  val ProjectNameSet        = ServerSideProc.Protocol.toEvents[String]
  val FieldMandatorinessMod = ServerSideProc.Protocol.toEvents[(CustomFieldId, Mandatory)]
  val ReqTypeImplicationMod = ServerSideProc.Protocol.toEvents[(CustomReqTypeId, ImplicationRequired)]
  val CreateContent         = ServerSideProc.Protocol.toEvents[CreateContentCmd]
  val UpdateContent         = ServerSideProc.Protocol.toEvents[UpdateContentCmd]
  val CustomIssueTypeCrud   = CrudProtocol[CustomIssueTypeId, (HashRefKey, Option[String])]
  val CustomReqTypeCrud     = CrudProtocol[CustomReqTypeId, (ReqType.Mnemonic, String, ImplicationRequired)]

  import InitAsync            .{pickleInstance => _i1}
  import ProjectNameSet       .{pickleInstance => _i2}
  import CustomIssueTypeCrud  .{pickleInstance => _i3}
  import CustomReqTypeCrud    .{pickleInstance => _i4}
  import FieldMandatorinessMod.{pickleInstance => _i5}
  import ReqTypeImplicationMod.{pickleInstance => _i6}
  import CreateContent        .{pickleInstance => _i7}
  import UpdateContent        .{pickleInstance => _i8}
  import TagCrud.Protocol     .{pickleInstance => _i9}
  import FieldCrud.Protocol   .{pickleInstance => _i10}

  final case class InitData(username       : Username,
                            projectMetaData: ProjectMetaData,
                            initAsync      : InitAsync            .Instance,
                            issueTypeCrud  : CustomIssueTypeCrud  .Instance,
                            reqTypeCrud    : CustomReqTypeCrud    .Instance,
                            reqTypeImpMod  : ReqTypeImplicationMod.Instance,
                            fieldMandMod   : FieldMandatorinessMod.Instance,
                            fieldCrud      : FieldCrud.Protocol   .Instance,
                            tagCrud        : TagCrud.Protocol     .Instance,
                            createContent  : CreateContent        .Instance,
                            updateContent  : UpdateContent        .Instance,
                            projectNameSet : ProjectNameSet       .Instance)
  implicit val picklerInitData = pickleCaseClass[InitData]

  final val EntryPointName = "P"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)

  final val CometListenerName = "C"
  val CometListener = ClientSideProc[VerifiedEvent.NonEmptySeq](CometListenerName)
}