package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react._
import monocle.macros._
import shipreq.base.util._
import shipreq.webapp.base.data.{FilterDead, HideDead, Project}
import shipreq.webapp.base.feature._
import shipreq.webapp.base.protocol.websocket.{ManualIssueCmd, UpdateConfigCmd, UpdateContentCmd}
import shipreq.webapp.base.ui.{ProjectItem, Toast}
import shipreq.webapp.client.project.app.pages.config.fields.FieldConfig
import shipreq.webapp.client.project.app.pages.config.issues.IssueConfig
import shipreq.webapp.client.project.app.pages.config.reqtypes.ReqTypeConfig
import shipreq.webapp.client.project.app.pages.config.tags.TagConfig
import shipreq.webapp.client.project.app.pages.content.issues.IssuesPage
import shipreq.webapp.client.project.app.pages.content.reqdetail.ReqDetail
import shipreq.webapp.client.project.app.pages.content.{reqdetail, reqtable}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{NewReqButton, ReqSearch}

sealed trait PreviewId
object PreviewId {

  final case class CF(id: CreateFeature.PreviewId) extends PreviewId
  final case class EF(id: EditorFeature.PreviewId) extends PreviewId

  implicit def equality: UnivEq[PreviewId] = UnivEq.derive
  implicit val reusability: Reusability[PreviewId] = Reusability.byUnivEq

  val ToCreate = Reusable.byRef(Intersection[PreviewId, CreateFeature.PreviewId] {
    case CF(e) => Some(e)
    case _: EF => None
  }(e => Some(CF(e))))

  val ToEditor = Reusable.byRef(Intersection[PreviewId, EditorFeature.PreviewId] {
    case EF(e) => Some(e)
    case _: CF => None
  }(e => Some(EF(e))))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

sealed abstract class AsyncKey
object AsyncKey {
  import reqdetail.Row.UseCaseSteps
  import shipreq.webapp.base.data.UseCaseStepId

  /** The req itself. Eg. if a req is being deleted then the entire req should be locked */
  case object WholeReq                             extends AsyncKey
  case class Editor(field: EditorFeature.FieldKey) extends AsyncKey
  case class UseCaseStepCtrls  (id: UseCaseStepId) extends AsyncKey
  case class AddUseCaseStep    (id: UseCaseStepId) extends AsyncKey
  case class AddUseCaseTailStep(s: UseCaseSteps)   extends AsyncKey

  @inline implicit def equality: UnivEq[AsyncKey] =
    UnivEq.derive

  implicit val reusability: Reusability[AsyncKey] =
    Reusability.byUnivEq

  val ToEditor = Intersection[AsyncKey, EditorFeature.FieldKey] {
    case Editor(key)           => Some(key)
    case WholeReq
       | UseCaseStepCtrls  (_)
       | AddUseCaseStep    (_)
       | AddUseCaseTailStep(_) => None
  }(e => Some(Editor(e)))

  val ToReqDetail = Intersection[AsyncKey, reqdetail.Cell] {
    case Editor(e) => e match {
      case f: EditorFeature.FieldKey.ForSomeReq  => Some(reqdetail.Cell.ReqField(f))
      case f: EditorFeature.FieldKey.UseCaseStep => Some(reqdetail.Cell.UseCaseStep(f.id))
      case EditorFeature.FieldKey.Code
         | EditorFeature.FieldKey.CodeGroupTitle
         | EditorFeature.FieldKey.ManualIssue(_) => None
    }
    case UseCaseStepCtrls  (id) => Some(reqdetail.Cell.UseCaseStepCtrls  (id))
    case AddUseCaseStep    (id) => Some(reqdetail.Cell.AddUseCaseStep    (id))
    case AddUseCaseTailStep(s)  => Some(reqdetail.Cell.AddUseCaseTailStep(s))
    case WholeReq               => Some(reqdetail.Cell.WholeReq)
  } {
    case reqdetail.Cell.WholeReq               => Some(WholeReq)
    case reqdetail.Cell.ReqField          (f)  => Some(Editor(f))
    case reqdetail.Cell.UseCaseStep       (id) => Some(Editor(EditorFeature.FieldKey.UseCaseStep(id)))
    case reqdetail.Cell.UseCaseStepCtrls  (id) => Some(UseCaseStepCtrls  (id))
    case reqdetail.Cell.AddUseCaseStep    (id) => Some(AddUseCaseStep    (id))
    case reqdetail.Cell.AddUseCaseTailStep(s)  => Some(AddUseCaseTailStep(s))
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

@Lenses
final case class State(projectName               : ProjectItem.WithEditableName.State,
                       reqSearch                 : ReqSearch.State,
                       reqLookup                 : String,
                       create                    : CreateFeature.State.ForProject,
                       newReqAsync               : AsyncFeature.State.D0[ErrorMsg],
                       edit                      : EditorFeature.State.ForProject,
                       editAsync                 : AsyncFeature.State.D2[EditorFeature.RowKey, AsyncKey, EditorFeature.AsyncError],
                       savedViews                : SavedViewFeature.State,
                       preview                   : PreviewFeature.State.Composite[PreviewId],
                       _filterDead               : FilterDead,
                       reqTable                  : reqtable.ReqTablePage.State,
                       reqDetail                 : ReqDetail.State,
                       newReqButton              : NewReqButton.State,
                       issuesPage                : IssuesPage.State,
                       toast                     : Toast.State,
                       updateConfigCmdAsync      : AsyncFeature.State.D1[UpdateConfigCmd, ErrorMsg], // TODO eh?
                       updateContentCmdAsync     : AsyncFeature.State.D1[UpdateContentCmd, ErrorMsg],
                       manualIssueCmdAsync       : AsyncFeature.State.D1[ManualIssueCmd, ErrorMsg],
                       fieldConfig               : FieldConfig.State,
                       fieldConfigAsync          : AsyncFeature.State.D0[ErrorMsg],
                       tagConfig                 : TagConfig.State,
                       tagConfigAsync            : AsyncFeature.State.D0[ErrorMsg],
                       reqTypeConfig             : ReqTypeConfig.State,
                       reqTypeConfigAsync        : AsyncFeature.State.D0[ErrorMsg],
                       customIssueTypeConfig     : IssueConfig.State,
                       customIssueTypeConfigAsync: AsyncFeature.State.D0[ErrorMsg],
                      ) {

  @inline def filterDead = _filterDead

  def setFilterDead(fd: FilterDead, p: Project): State =
    if (fd ==* _filterDead)
      this
    else
      copy(
        _filterDead = fd,
        savedViews = savedViews.setFilterDead(fd, p),
      )
}

object State {

  val recorder = ErrorHandlingFeature.StateRecorder[State]

  def init(p: Project): State =
    State(
      projectName                = ProjectItem.WithEditableName.State.init,
      reqSearch                  = ReqSearch.State.init,
      reqLookup                  = "",
      create                     = CreateFeature.State.ForProject.init,
      newReqAsync                = AsyncFeature.State.initD0,
      edit                       = EditorFeature.State.initForProject,
      editAsync                  = AsyncFeature.State.initD2,
      savedViews                 = SavedViewFeature.State.init(p),
      preview                    = PreviewFeature.State.Composite.init,
      _filterDead                = p.savedViews.map(_.default.view.filterDead).getOrElse(HideDead),
      reqTable                   = reqtable.ReqTablePage.State.init,
      reqDetail                  = ReqDetail.State.init,
      newReqButton               = None,
      issuesPage                 = IssuesPage.State.init,
      toast                      = Toast.State.init,
      updateConfigCmdAsync       = AsyncFeature.State.initD1,
      updateContentCmdAsync      = AsyncFeature.State.initD1,
      manualIssueCmdAsync        = AsyncFeature.State.initD1,
      fieldConfig                = FieldConfig.initState,
      fieldConfigAsync           = AsyncFeature.State.initD0,
      tagConfig                  = TagConfig.initState,
      tagConfigAsync             = AsyncFeature.State.initD0,
      reqTypeConfig              = ReqTypeConfig.initState,
      reqTypeConfigAsync         = AsyncFeature.State.initD0,
      customIssueTypeConfig      = IssueConfig.initState,
      customIssueTypeConfigAsync = AsyncFeature.State.initD0,
    )

  implicit val reusability: Reusability[State] =
    Reusability.byRef

  val savedViewAsync =
    savedViews ^|-> SavedViewFeature.State.async
}
