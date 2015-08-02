package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.equal.ToEqualOps
import shipreq.base.util.UnivEq.univEqOption
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ReqCodeGroupGD
import shipreq.webapp.base.protocol.CreateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.base.util.GenericDataMacros._
import shipreq.webapp.client.app.ui.SelectOne.{Choice, Choices}
import shipreq.webapp.client.app.ui.reqtable.edit.{ReqCodeEditor, RichTextEditor}
import shipreq.webapp.client.app.ui.{ProjectWidgets, SelectOne, VUCA}
import shipreq.webapp.client.lib.TIO
import shipreq.webapp.client.util.Enabled

object CreationInterface {
  sealed trait Type
  case object ReqCodeGroupType extends Type
  //case class GenericReqType(rt: CustomReqTypeId) extends Type
  implicit def typeEquality: UnivEq[Type] = UnivEq.force

  type SelType = Option[Type]
  val selectComponent = SelectOne.Component[SelType]

  case class Props(createIO: (CreateContentCmd, TIO.Success, String => TIO.Failure) => IO[Unit], state: State)

  @Lenses
  case class State(selectedType: SelType,
                   types       : Choices[SelType],
                   rcg         : CreateReqCodeGroupState)

  sealed trait Status
  case object Editing extends Status
  case object Locked extends Status
  case class Failed(reason: String) extends Status
  implicit def statusEquality: UnivEq[Status] = UnivEq.force

  @Lenses
  case class CreateReqCodeGroupState(status: Status, reqCode: String, title: String)

  implicit val reusabilityState: Reusability[State] = Reusability.byRef
  //  implicit val reusabilityProps: Reusability[Props] = Reusability.by(_.state)

  def initState: State =
    State(None, initChoices, CreateReqCodeGroupState(Editing, "", ""))

  def initChoices: Choices[SelType] =
    NonEmptyVector.varargs(
      Choice(None, "", Enabled),
      Choice(Some(ReqCodeGroupType), UiText.reqCodeGroup, Enabled))
}

// =====================================================================================================================
import shipreq.webapp.client.app.ui.reqtable.CreationInterface._

class CreationInterface($             : CompStateFocus[State],
                        project       : Px[Project],
                        projectText   : Px[PlainText.ForProject],
                        projectWidgets: Px[ProjectWidgets],
                        textSearch    : Px[TextSearch]) {

  private def render(p: Props) = {
    val s = p.state

    val select: SelType => IO[Unit] =
      $ _setStateL State.selectedType

    val selProps = SelectOne.Props[SelType](
      s.selectedType, s.types, Some(select))

    val detail: Type => TagMod = {
      case ReqCodeGroupType => CreateReqCodeGroup.Component(p)
    }

    <.div(
      "Create ",
      selectComponent(selProps),
      s.selectedType map detail)
  }

  val Component = ReactComponentB[Props]("Creation")
    .stateless
    .render($ => render($.props))
//    .configure(shouldComponentUpdate)
    .build

  object CreateReqCodeGroup { // ---------------------------------------------------------------------------------------

    val $$ = $ focusStateL State.rcg
    val setStatus  = $$ focusStateL CreateReqCodeGroupState.status  setStateIO (_: Status)
    val setReqCode = $$ focusStateL CreateReqCodeGroupState.reqCode setStateIO (_: String)
    val setTitle   = $$ focusStateL CreateReqCodeGroupState.title   setStateIO (_: String)

    val mkPropsReqCode = ReqCodeEditor.ForGroup.prepare(None, project.map(_.reqCodes.trie))
    val mkPropsTitle   = RichTextEditor.ReqCodeGroupTitle.prepare(project, projectText, projectWidgets, textSearch)

    def render(p: Props) = {
      val state = p.state.rcg

      val propsReqCode = mkPropsReqCode(VUCA.vu(state.reqCode, setReqCode))
      val propsTitle   = mkPropsTitle  (VUCA.vu(state.title,   setTitle))

      val create: Option[IO[Unit]] =
        for {
          code  <- propsReqCode.parseResult.toOption
          title <- propsTitle.parseResult.toOption
          if state.status ≠ Locked
        } yield IO {
          val cmd = CreateContentCmd.CreateReqCodeGroup(gdAllValues(ReqCodeGroupGD, ""))
          val remoteCall = p.createIO(cmd,
            TIO.Success(setStatus(Editing)),
            f => TIO.Failure(setStatus(Failed(f))))
          remoteCall >> setStatus(Locked)
        }.join

      def createButton =
        <.button(
          ^.disabled := create.isEmpty,
          ^.onClick ~~>? create,
          "Create") // english

      def failureNotice: Option[TagMod] =
        state.status match {
          case Editing | Locked => None
          case Failed(err) => Some(
            <.div(err, <.button("Got it", ^.onClick ~~> setStatus(Editing)))) // English
        }

      <.table(
        <.thead(
          <.tr(
            <.th(UiText.ColumnNames.code),
            <.th(UiText.ColumnNames.title),
            <.th())),
        <.tbody(
          <.tr(
            <.td(propsReqCode.render),
            <.td(propsTitle.render),
            <.td(createButton, failureNotice))))
    }

    val Component = ReactComponentB[Props]("CreateRCG")
      .stateless
      .render($ => render($.props))
      .build
  }
}
