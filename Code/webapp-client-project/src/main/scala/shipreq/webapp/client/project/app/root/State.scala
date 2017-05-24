package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.extra._
import monocle.macros._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, HideDead}
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.ui.ProjectItem
import shipreq.webapp.client.project.app.state.ClientData
import shipreq.webapp.client.project.app.{reqdetail, reqtable2}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import reqdetail.ReqDetail

sealed trait PreviewId
object PreviewId {

  case class Editor(id: EditorFeature.PreviewId) extends PreviewId

  implicit def equality: UnivEq[PreviewId] = UnivEq.derive
  implicit val reusability: Reusability[PreviewId] = Reusability.byUnivEq

  val ToEditor = Intersection[PreviewId, EditorFeature.PreviewId] {
    case Editor(e) => Some(e)
  }(e => Some(Editor(e)))
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
         | EditorFeature.FieldKey.CodeGroupTitle => None
    }
    case UseCaseStepCtrls  (id) => Some(reqdetail.Cell.UseCaseStepCtrls  (id))
    case AddUseCaseStep    (id) => Some(reqdetail.Cell.AddUseCaseStep    (id))
    case AddUseCaseTailStep(s)  => Some(reqdetail.Cell.AddUseCaseTailStep(s))
    case WholeReq               => None // TODO ReqDetail doesn't lock the whole requirement when deleting
  } {
    case reqdetail.Cell.ReqField          (f)  => Some(Editor(f))
    case reqdetail.Cell.UseCaseStep       (id) => Some(Editor(EditorFeature.FieldKey.UseCaseStep(id)))
    case reqdetail.Cell.UseCaseStepCtrls  (id) => Some(UseCaseStepCtrls  (id))
    case reqdetail.Cell.AddUseCaseStep    (id) => Some(AddUseCaseStep    (id))
    case reqdetail.Cell.AddUseCaseTailStep(s)  => Some(AddUseCaseTailStep(s))
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

@Lenses
case class State(projectName : ProjectItem.WithEditableName.State,
                 reqLookup   : String,
                 editors     : EditorFeature.State.ForProject,
                 async       : AsyncFeature.State.D2[EditorFeature.RowKey, AsyncKey, EditorFeature.AsyncError],
                 preview     : PreviewFeature.State[PreviewId],
                 filterDead  : FilterDead,
                 reqTable    : reqtable2.ReqTablePage.State,
                 reqDetail   : ReqDetail.State)

object State {
  def init(cd: ClientData): State =
    State(
      ProjectItem.WithEditableName.State.init,
      "",
      EditorFeature.State.initForProject,
      AsyncFeature.State.initD2,
      PreviewFeature.State.init,
      HideDead,
      reqtable2.ReqTablePage.State.init,
      ReqDetail.initState)

  val reqTableSettings = State.reqTable ^|-> reqtable2.ReqTablePage.State.tableSettings
}
