package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.extra._
import monocle.macros._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, HideDead}
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.ui.ProjectItem
import shipreq.webapp.client.project.app.state.ClientData
import shipreq.webapp.client.project.app.{reqdetail, reqtable}
import shipreq.webapp.client.project.feature.ContentEditorFeature.EditFieldKey
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import reqdetail.ReqDetail
import reqtable.ReqTable

sealed trait FocusId
object FocusId {

  case class Content(row: reqtable.Row.SourceId, f: EditFieldKey) extends FocusId
  case class ReqTableCI(value: reqtable.FocusId.InCI) extends FocusId

  implicit def equality: UnivEq[FocusId] = UnivEq.derive
  implicit val reusability: Reusability[FocusId] = Reusability.byUnivEq

  val ToReqTable = Intersection[FocusId, reqtable.FocusId] {
    case Content(r, f) => reqtable.Column.EditFieldKeyIntersection.reverse.getOptionMap(f, reqtable.FocusId.AtCell(r, _))
    case ReqTableCI(a) => Some(a)
  } {
    case reqtable.FocusId.AtCell(r, c) => reqtable.Column.EditFieldKeyIntersection.getOptionMap(c, Content(r, _))
    case a: reqtable.FocusId.InCI      => Some(ReqTableCI(a))
  }
}

sealed abstract class AsyncKey
object AsyncKey {
  import reqdetail.Row.UseCaseSteps
  import shipreq.webapp.base.data.{CustomFieldId, Dead, Live, UseCaseStepId}

  /** The req itself. Eg. if a req is being deleted then the entire req should be locked */
  case object WholeReq                             extends AsyncKey
  case object ReqType                              extends AsyncKey
  case object Code                                 extends AsyncKey
  case object Title                                extends AsyncKey
  case object Tags                                 extends AsyncKey
  case class Implications      (dir: Direction)    extends AsyncKey
  case class CustomField       (id: CustomFieldId) extends AsyncKey
  case class UseCaseStep       (id: UseCaseStepId) extends AsyncKey
  case class UseCaseStepCtrls  (id: UseCaseStepId) extends AsyncKey
  case class AddUseCaseStep    (id: UseCaseStepId) extends AsyncKey
  case class AddUseCaseTailStep(s: UseCaseSteps)   extends AsyncKey

  object Implications {
    private val memo = Direction.memo(new Implications(_))
    def apply(d: Direction): Implications = memo(d)
  }

  @inline implicit def equality: UnivEq[AsyncKey] =
    UnivEq.derive

  implicit val reusability: Reusability[AsyncKey] =
    Reusability.byUnivEq

  val ToReqTable = Intersection[AsyncKey, reqtable.Column] {
    case ReqType           => Some(reqtable.Column.ReqType)
    case Code              => Some(reqtable.Column.Code)
    case Title             => Some(reqtable.Column.Title)
    case Tags              => Some(reqtable.Column.Tags)
    case Implications(dir) => Some(reqtable.Column.Implications(dir))
    case CustomField(id)   => Some(reqtable.Column.CustomField(id, Live))
    case WholeReq
       | UseCaseStep       (_)
       | UseCaseStepCtrls  (_)
       | AddUseCaseStep    (_)
       | AddUseCaseTailStep(_) => None
  } {
    case reqtable.Column.ReqType               => Some(ReqType)
    case reqtable.Column.Code                  => Some(Code)
    case reqtable.Column.Title                 => Some(Title)
    case reqtable.Column.Tags                  => Some(Tags)
    case reqtable.Column.Implications(dir)     => Some(Implications(dir))
    case reqtable.Column.CustomField(id, Live) => Some(CustomField(id))
    case reqtable.Column.Pubid
       | reqtable.Column.DeletionReason
       | reqtable.Column.CustomField(_, Dead)  => None
  }

  val ToReqTable2 = Intersection[AsyncKey, Option[reqtable.Column]] {
    case WholeReq => Some(None)
    case x        => ToReqTable.getOption(x).map(Some(_))
  } {
    case None    => Some(WholeReq)
    case Some(x) => ToReqTable.reverse.getOption(x)
  }

  val ToReqDetail = Intersection[AsyncKey, reqdetail.Cell] {
    case ReqType               => Some(reqdetail.Cell.ReqType)
    case Code                  => Some(reqdetail.Cell.Code)
    case Title                 => Some(reqdetail.Cell.Title)
    case Tags                  => Some(reqdetail.Cell.Tags)
    case Implications(dir)     => Some(reqdetail.Cell.Implications(dir))
    case CustomField(id)       => Some(reqdetail.Cell.CustomField(id))
    case UseCaseStep(id)       => Some(reqdetail.Cell.UseCaseStep(id))
    case UseCaseStepCtrls(id)  => Some(reqdetail.Cell.UseCaseStepCtrls(id))
    case AddUseCaseStep(id)    => Some(reqdetail.Cell.AddUseCaseStep(id))
    case AddUseCaseTailStep(s) => Some(reqdetail.Cell.AddUseCaseTailStep(s))
    case WholeReq              => None // TODO ReqDetail doesn't lock the whole requirement when deleting
  } {
    case reqdetail.Cell.ReqType               => Some(ReqType)
    case reqdetail.Cell.Code                  => Some(Code)
    case reqdetail.Cell.Title                 => Some(Title)
    case reqdetail.Cell.Tags                  => Some(Tags)
    case reqdetail.Cell.Implications(dir)     => Some(Implications(dir))
    case reqdetail.Cell.CustomField(id)       => Some(CustomField(id))
    case reqdetail.Cell.UseCaseStep(id)       => Some(UseCaseStep(id))
    case reqdetail.Cell.UseCaseStepCtrls(id)  => Some(UseCaseStepCtrls(id))
    case reqdetail.Cell.AddUseCaseStep(id)    => Some(AddUseCaseStep(id))
    case reqdetail.Cell.AddUseCaseTailStep(s) => Some(AddUseCaseTailStep(s))
  }
}

@Lenses
case class State(projectName : ProjectItem.WithEditableName.State,
                 reqLookup   : String,
                 editStates  : ContentEditorFeature.D2.State.Simple[reqtable.Row.SourceId, EditFieldKey],
                 asyncStates : AsyncActionFeature.D2.State.Simple[reqtable.Row.SourceId, AsyncKey, String],
                 previewState: PreviewFeature.State[FocusId],
                 filterDead  : FilterDead,
                 reqTable    : ReqTable.State,
                 reqDetail   : ReqDetail.State)

object State {
  def init(cd: ClientData): State =
    State(
      ProjectItem.WithEditableName.State.init,
      "",
      ContentEditorFeature.D2.State.init,
      AsyncActionFeature.D2.State.init,
      PreviewFeature.State.init,
      HideDead,
      ReqTable.State.init(cd, HideDead, None),
      ReqDetail.initState)

  val reqTableVS = State.reqTable ^|-> ReqTable.State.viewSettings
}
