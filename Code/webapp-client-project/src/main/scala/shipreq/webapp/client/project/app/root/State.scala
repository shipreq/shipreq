package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.extra._
import monocle.macros._
import scalaz.\/-
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, HideDead}
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.ui.ProjectItem
import shipreq.webapp.client.project.app.state.ClientData
import shipreq.webapp.client.project.app.{reqdetail, reqtable}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import reqdetail.ReqDetail
import reqtable.ReqTable

sealed trait FocusId // TODO Rename all of this to PreviewId
object FocusId {

  case class Editor(id: EditorFeature.PreviewId) extends FocusId
  case class ReqTableCI(value: reqtable.FocusId.InCI) extends FocusId

  implicit def equality: UnivEq[FocusId] = UnivEq.derive
  implicit val reusability: Reusability[FocusId] = Reusability.byUnivEq

  val ToReqTable = Intersection[FocusId, reqtable.FocusId] {
    case Editor(e)     => Some(reqtable.FocusId.InEditor(e))
    case ReqTableCI(a) => Some(a)
  } {
    case reqtable.FocusId.InEditor(e) => Some(Editor(e))
    case a: reqtable.FocusId.InCI     => Some(ReqTableCI(a))
  }

  val ToEditor = Intersection[FocusId, EditorFeature.PreviewId] {
    case Editor(e)     => Some(e)
    case ReqTableCI(_) => None
  }(e => Some(Editor(e)))
}

sealed abstract class AsyncKey
object AsyncKey {
  import reqdetail.Row.UseCaseSteps
  import shipreq.webapp.base.data.UseCaseStepId

  /** The req itself. Eg. if a req is being deleted then the entire req should be locked */
  case object WholeReq                             extends AsyncKey
  case class Editor(cell: EditorFeature.CellKey)   extends AsyncKey
  case class UseCaseStepCtrls  (id: UseCaseStepId) extends AsyncKey
  case class AddUseCaseStep    (id: UseCaseStepId) extends AsyncKey
  case class AddUseCaseTailStep(s: UseCaseSteps)   extends AsyncKey

  @inline implicit def equality: UnivEq[AsyncKey] =
    UnivEq.derive

  implicit val reusability: Reusability[AsyncKey] =
    Reusability.byUnivEq

  val ToEditor = Intersection[AsyncKey, EditorFeature.CellKey] {
    case Editor(key)           => Some(key)
    case WholeReq
       | UseCaseStepCtrls  (_)
       | AddUseCaseStep    (_)
       | AddUseCaseTailStep(_) => None
  }(e => Some(Editor(e)))

  val ToReqTable = Intersection[AsyncKey, reqtable.Column] {
    case Editor(key)           => reqtable.Column.editorCellIntersection.reverse.getOption(key)
    case WholeReq
       | UseCaseStepCtrls  (_)
       | AddUseCaseStep    (_)
       | AddUseCaseTailStep(_) => None
  }(reqtable.Column.editorCellIntersection.getOption(_).map(Editor))

  val ToReqTable2 = Intersection[AsyncKey, Option[reqtable.Column]] {
    case WholeReq => Some(None)
    case x        => ToReqTable.getOption(x).map(Some(_))
  } {
    case None    => Some(WholeReq)
    case Some(x) => ToReqTable.reverse.getOption(x)
  }

  val ToReqDetail = Intersection[AsyncKey, reqdetail.Cell] {
    case Editor(e) => e match {
      case EditorFeature.CellKey.ReqType                => Some(reqdetail.Cell.ReqType               )
      case EditorFeature.CellKey.Code                   => Some(reqdetail.Cell.Code                  )
      case EditorFeature.CellKey.Title                  => Some(reqdetail.Cell.Title                 )
      case EditorFeature.CellKey.CustomTextField(field) => Some(reqdetail.Cell.CustomTextField(field))
      case EditorFeature.CellKey.Tags           (field) => Some(reqdetail.Cell.Tags           (field))
      case EditorFeature.CellKey.Implications   (scope) => Some(reqdetail.Cell.Implications   (scope))
      case EditorFeature.CellKey.UseCaseStep    (id)    => Some(reqdetail.Cell.UseCaseStep    (id)   )
    }
    case UseCaseStepCtrls  (id) => Some(reqdetail.Cell.UseCaseStepCtrls  (id))
    case AddUseCaseStep    (id) => Some(reqdetail.Cell.AddUseCaseStep    (id))
    case AddUseCaseTailStep(s)  => Some(reqdetail.Cell.AddUseCaseTailStep(s) )
    case WholeReq               => None // TODO ReqDetail doesn't lock the whole requirement when deleting
  } {
    case reqdetail.Cell.ReqType                => Some(Editor(EditorFeature.CellKey.ReqType               ))
    case reqdetail.Cell.Code                   => Some(Editor(EditorFeature.CellKey.Code                  ))
    case reqdetail.Cell.Title                  => Some(Editor(EditorFeature.CellKey.Title                 ))
    case reqdetail.Cell.CustomTextField(field) => Some(Editor(EditorFeature.CellKey.CustomTextField(field)))
    case reqdetail.Cell.Tags           (field) => Some(Editor(EditorFeature.CellKey.Tags           (field)))
    case reqdetail.Cell.Implications   (scope) => Some(Editor(EditorFeature.CellKey.Implications   (scope)))
    case reqdetail.Cell.UseCaseStep    (id)    => Some(Editor(EditorFeature.CellKey.UseCaseStep    (id)   ))
    case reqdetail.Cell.UseCaseStepCtrls  (id) => Some(UseCaseStepCtrls  (id))
    case reqdetail.Cell.AddUseCaseStep    (id) => Some(AddUseCaseStep    (id))
    case reqdetail.Cell.AddUseCaseTailStep(s)  => Some(AddUseCaseTailStep(s) )
  }
}

@Lenses
case class State(projectName : ProjectItem.WithEditableName.State,
                 reqLookup   : String,
                 editors     : EditorFeature.State.ForProject,
                 async       : AsyncFeature.State.D2[EditorFeature.RowKey, AsyncKey, EditorFeature.AsyncError],
                 preview     : PreviewFeature.State[FocusId],
                 filterDead  : FilterDead,
                 reqTable    : ReqTable.State,
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
      ReqTable.State.init(cd, HideDead, None),
      ReqDetail.initState)

  val reqTableVS = State.reqTable ^|-> ReqTable.State.viewSettings
}
