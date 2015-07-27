package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scalaz.effect.IO
import shipreq.base.util.{UnivEq, NonEmptyVector}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.SelectOne
import shipreq.webapp.client.util.Enabled
import SelectOne.{Choice, Choices}
import UnivEq.univEqOption

object CreationInterface {

  sealed trait Type
  case object ReqCodeGroupType extends Type
  //case class GenericReqType(rt: CustomReqTypeId) extends Type
  implicit def typeEquality: UnivEq[Type] = UnivEq.force

  type SelType = Option[Type]
  val selectComponent = SelectOne.Component[SelType]

  @Lenses
  case class State(selectedType: SelType,
                   types       : Choices[SelType],
                   rcgState    : CreateReqCodeGroup.State)

  case class Props(state: ReusableVar[State])

  implicit val reusabilityState: Reusability[State] = Reusability.byRef
  implicit val reusabilityProps: Reusability[Props] = Reusability.by(_.state)

  def initChoices: Choices[SelType] =
    NonEmptyVector.varargs(
      Choice(None, "", Enabled),
      Choice(Some(ReqCodeGroupType), UiText.reqCodeGroup, Enabled))

  def initState: State =
    State(None, initChoices, CreateReqCodeGroup.initState)

  def render(p: Props) = {
    val s = p.state.value

    val select: SelType => IO[Unit] =
      p.state setL State.selectedType

    val selProps = SelectOne.Props[SelType](
      s.selectedType, s.types, Some(select))

    val detail: Type => TagMod = {
      case ReqCodeGroupType =>
        CreateReqCodeGroup.Component(ExternalVar(s.rcgState)(p.state setL State.rcgState))
    }

    <.div(
      "Create ",
      selectComponent(selProps),
      s.selectedType map detail)
  }

  val Component = ReactComponentB[Props]("Creation")
    .stateless
    .render($ => render($.props))
    .configure(shouldComponentUpdate)
    .build

  // ===================================================================================================================
  object CreateReqCodeGroup {

    type Props = ExternalVar[State]
    case class State()

    def initState: State =
      State()

    def render(p: Props) = {
      <.table(
        <.tbody(
          <.tr(
            <.th(UiText.ColumnNames.code),
            <.td("???")),
          <.tr(
            <.th(UiText.ColumnNames.title),
            <.td("???"))))
    }

    val Component = ReactComponentB[Props]("CreateRCG")
      .stateless
      .render($ => render($.props))
      .build
  }
}
