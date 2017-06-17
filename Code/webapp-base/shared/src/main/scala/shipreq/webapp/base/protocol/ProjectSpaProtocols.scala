package shipreq.webapp.base.protocol

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.data._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecData._
import BinCodecEvents._

/**
  * Protocols for the Project SPA / webapp-client-project module.
  */
object ProjectSpaProtocols {

  val CustomIssueTypeCrud   = CrudProtocol[CustomIssueTypeId, (HashRefKey, Option[String])]
  val CustomReqTypeCrud     = CrudProtocol[CustomReqTypeId, (ReqType.Mnemonic, String, ImplicationRequired)]
  val ProjectInit           = ServerSideProc.Protocol[Unit, Project]
  val ProjectNameSet        = ServerSideProc.Protocol.toEvents[String]
  val FieldMandatorinessMod = ServerSideProc.Protocol.toEvents[(CustomFieldId, Mandatory)]
  val ReqTypeImplicationMod = ServerSideProc.Protocol.toEvents[(CustomReqTypeId, ImplicationRequired)]
  val CreateContent         = ServerSideProc.Protocol.toEvents[CreateContentCmd]
  val UpdateContent         = ServerSideProc.Protocol.toEvents[UpdateContentCmd]

  import ProjectInit          .{pickleInstance => _i1}
  import ProjectNameSet       .{pickleInstance => _i2}
  import CustomIssueTypeCrud  .{pickleInstance => _i3}
  import CustomReqTypeCrud    .{pickleInstance => _i4}
  import FieldMandatorinessMod.{pickleInstance => _i5}
  import ReqTypeImplicationMod.{pickleInstance => _i6}
  import CreateContent        .{pickleInstance => _i7}
  import UpdateContent        .{pickleInstance => _i8}
  import TagCrud.Protocol     .{pickleInstance => _i9}
  import FieldCrud.Protocol   .{pickleInstance => _i10}

  final case class InitClient(username      : Username,
                              project       : ProjectCatalogue.Item,
                              projectInit   : ProjectInit          .Instance,
                              issueTypeCrud : CustomIssueTypeCrud  .Instance,
                              reqTypeCrud   : CustomReqTypeCrud    .Instance,
                              reqTypeImpMod : ReqTypeImplicationMod.Instance,
                              fieldMandMod  : FieldMandatorinessMod.Instance,
                              fieldCrud     : FieldCrud.Protocol   .Instance,
                              tagCrud       : TagCrud.Protocol     .Instance,
                              createContent : CreateContent        .Instance,
                              updateContent : UpdateContent        .Instance,
                              projectNameSet: ProjectNameSet       .Instance)

  implicit val picklerInitClient = pickleCaseClass[InitClient]

  final val EntryPointName = "P"
  val EntryPoint = ClientSideProc[InitClient](EntryPointName)

  final val CometListenerName = "C"
  val CometListener = ClientSideProc[NonEmptyVector[VerifiedEvent]](CometListenerName)
}