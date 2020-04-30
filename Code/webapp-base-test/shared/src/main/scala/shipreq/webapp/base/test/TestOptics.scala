package shipreq.webapp.base.test

import monocle._
import monocle.std.option.{some => atSome}
import shipreq.base.util.{VectorTree, IMap}
import shipreq.webapp.base.data.{Optional => _, _}
import shipreq.webapp.base.text.Text

object TestOptics {

  val customReqTypesLive: Traversal[Project, Live] =
    Project.reqTypes ^|->
    ReqTypes.custom  ^|->>
    IMap.traversal   ^|->
    CustomReqType.live


  import ReqCode._

  val reqCodeDataDeadGroup = Lens[Data, DeadGroup](_.deadGroup)(dg => {
    case d: Inactive    => d.copy(deadGroup = dg)
    case d: ActiveReq   => d.copy(deadGroup = dg)
    case d: ActiveGroup => d
  })

  private val reqCodeDataDeadGroupSome = reqCodeDataDeadGroup ^<-? atSome

  val reqCodeDataDeadGroupId: Optional[Data, ReqCodeGroupId] =
    reqCodeDataDeadGroupSome ^|-> DeadCodeGroup.id

  val reqCodeActiveGroupId: Lens[ActiveGroup, ReqCodeGroupId] =
    ActiveGroup.group ^|-> LiveCodeGroup.id

  private val reqCodeActiveGroupTitle: Lens[ActiveGroup, Text.CodeGroupTitle.OptionalText] =
    ActiveGroup.group ^|-> LiveCodeGroup.title

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
    case d: ActiveGroup => reqCodeActiveGroupTitle.set(n)(d)
    case d: ActiveReq   => d.copy(deadGroup = d.deadGroup.map(DeadCodeGroup.title set n))
    case d: Inactive    => d.copy(deadGroup = d.deadGroup.map(DeadCodeGroup.title set n))
  })

  private val reqCodeTrieFixK = Trie.fixk
  val reqCodeTrieValueTraversal: Traversal[Trie, Data] =
    PTraversal.fromTraverse[reqCodeTrieFixK.Trie, Data, Data](reqCodeTrieFixK.traverseTrie)

  val genericReqTitlesInReqs: Traversal[Requirements, Text.GenericReqTitle.OptionalText] =
    Requirements.genericReqs ^|-> GenericReqs.imap ^|->> IMap.traversal[GenericReqId, GenericReq] ^|-> GenericReq.title

  val useCasesInReqs: Traversal[Requirements, UseCase] =
    Requirements.useCases ^|-> UseCases.imap ^|->> IMap.traversal[UseCaseId, UseCase]

  val useCaseStepTextsInUseCase: Traversal[UseCase, Text.UseCaseStep.OptionalText] =
    UseCase.stepsTraversal ^|-> UseCaseSteps.tree ^|->> VectorTree.traversal ^|-> UseCaseStep.titleExplicitly

  val grsLive: Traversal[Requirements, Live] =
    Requirements.genericReqs ^|-> GenericReqs.imap ^|->> IMap.traversal[GenericReqId, GenericReq] ^|-> GenericReq.liveExplicitly

  val ucsLive: Traversal[Requirements, Live] =
    useCasesInReqs ^|-> UseCase.liveExplicitly
}
