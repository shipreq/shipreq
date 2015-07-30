package shipreq.webapp.client.app.ui.reqtable.edit

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra.Px
import org.scalajs.dom.raw.HTMLSelectElement
import scala.scalajs.js.{UndefOr, undefined}
import scalaz.Equal
import scalaz.effect.IO
import scalaz.syntax.equal._
import shipreq.base.util.NonEmptySet
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.{RemoteDataEditor, SelectOne}
import shipreq.webapp.client.app.ui.reqtable.Cell
import shipreq.webapp.client.lib.TIO
import shipreq.webapp.client.util.Enabled
import SelectOne.Choice
import UpdateContentCmd.SetGenericReqType

object ReqTypeSelector {

  type A = CustomReqType

  private implicit val equality: Equal[A] = Equal.equalBy(_.id)

  def apply(initial  : A,
            subjectId: GenericReqId,
            fields   : Px[Set[A]])
           (setSelf  : RemoteDataEditor.SetOpState,
            onCommit0: UpdateContentOnCommit): Cell.State = {

    val fieldsN = fields
      .map(_.filter(_.live :: Live))
      .map(NonEmptySet(initial, _))

    val onCommit = onCommit0.cmapToInitial(initial.id)(SetGenericReqType(subjectId, _)).cmap[A](_.id)

    def commitIfChanged(a: A, commit: RemoteDataEditor.CommitFn): UndefOr[TIO.Commit] =
      if (a.id ≟ initial.id)
        undefined
      else
        commit(onCommit(a))

    RemoteDataEditor.opDefault[A, A](
      initial, identity, setSelf,
      (s, u, abort, commit) =>
        component(Props(s, u, abort, commitIfChanged(s, commit), fieldsN.value())))
  }

  val selectComp = SelectOne.Component[A]

  private val selectRef = Ref[HTMLSelectElement]("i")

  case class Props(state      : A,
                   stateUpdate: A => IO[Unit],
                   abort      : TIO.Abort,
                   commit     : UndefOr[TIO.Commit],
                   fields     : NonEmptySet[A])

  val component = ReactComponentB[Props]("ReqTypeSelector")
    .render(render(_))
    .componentDidMount(selectRef(_).tryFocus())
    .build

  def render(p: Props): ReactElement = {
    def choice(a: A) = Choice[A](a, a.fullName, Enabled)

    val choices = p.fields.mapV(choice).sortBy(_.label)

    val selector = selectComp.set(ref = selectRef)(SelectOne.Props(p.state, choices, Some(p.stateUpdate)))

    val button =
      p.commit.fold[ReactTag](
        <.button(UiText.buttonAbortChange,  ^.onClick ~~> p.abort))(
        io => <.button(UiText.buttonCommitChange, ^.onClick ~~> io))

    <.div(selector, button)
  }
}
