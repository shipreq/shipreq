package shipreq.webapp.member.test.project

import monocle._
import monocle.std.option.{some => atSome}
import shipreq.base.util.{IMap, VectorTree}
import shipreq.webapp.member.project.data.{Optional => _, _}
import shipreq.webapp.member.project.text.Text

object TestOptics {

  val customReqTypesLive: Traversal[Project, Live] =
    Project.reqTypes andThen
    ReqTypes.custom andThen
    IMap.traversal[CustomReqTypeId,CustomReqType] andThen
    CustomReqType.live

  import ReqCode._

  val reqCodeDataDeadGroup = Lens[Data, DeadGroup](_.deadGroup)(dg => {
    case d: Inactive    => d.copy(deadGroup = dg)
    case d: ActiveReq   => d.copy(deadGroup = dg)
    case d: ActiveGroup => d
  })

  private val reqCodeDataDeadGroupSome =
    reqCodeDataDeadGroup andThen atSome[DeadCodeGroup]

  val reqCodeDataDeadGroupId: Optional[Data, ReqCodeGroupId] =
    reqCodeDataDeadGroupSome andThen DeadCodeGroup.id

  val reqCodeActiveGroupId: Lens[ActiveGroup, ReqCodeGroupId] =
    ActiveGroup.group andThen LiveCodeGroup.id

  private val reqCodeActiveGroupTitle: Lens[ActiveGroup, Text.CodeGroupTitle.OptionalText] =
    ActiveGroup.group andThen LiveCodeGroup.title

  val reqCodeDataReqInactive = Lens[Data, ReqInactive](_.reqInactive)(n => {
    case d: ActiveReq   => d.copy(reqInactive = n)
    case d: ActiveGroup => d.copy(reqInactive = n)
    case d: Inactive    => d.copy(reqInactive = n)
  })

  val reqCodeDataGroupTitle = Optional[Data, Text.CodeGroupTitle.OptionalText]({
    case d: ActiveGroup => Some(d.group.title)
    case d: ActiveReq   => d.deadGroup.map(_.title)
    case d: Inactive    => d.deadGroup.map(_.title)
  })(n => {
    case d: ActiveGroup => reqCodeActiveGroupTitle.replace(n)(d)
    case d: ActiveReq   => d.copy(deadGroup = d.deadGroup.map(DeadCodeGroup.title replace n))
    case d: Inactive    => d.copy(deadGroup = d.deadGroup.map(DeadCodeGroup.title replace n))
  })

  private val reqCodeTrieFixK = Trie.fixk
  val reqCodeTrieValueTraversal: Traversal[Trie, Data] =
    PTraversal.fromTraverse[reqCodeTrieFixK.Trie, Data, Data](reqCodeTrieFixK.traverseTrie)

  val genericReqTitlesInReqs: Traversal[Requirements, Text.GenericReqTitle.OptionalText] =
    Requirements.genericReqs andThen GenericReqs.imap andThen IMap.traversal[GenericReqId, GenericReq] andThen GenericReq.title

  val useCasesInReqs: Traversal[Requirements, UseCase] =
    Requirements.useCases andThen UseCases.imap andThen IMap.traversal[UseCaseId, UseCase]

  val useCaseStepTextsInUseCase: Traversal[UseCase, Text.UseCaseStep.OptionalText] =
    UseCase.stepsTraversal andThen
    UseCaseSteps.tree andThen
    VectorTree.traversal[UseCaseStep] andThen
    UseCaseStep.titleExplicitly

  val grsLive: Traversal[Requirements, Live] =
    Requirements.genericReqs andThen GenericReqs.imap andThen IMap.traversal[GenericReqId, GenericReq] andThen GenericReq.liveExplicitly

  val ucsLive: Traversal[Requirements, Live] =
    useCasesInReqs andThen UseCase.liveExplicitly
}
