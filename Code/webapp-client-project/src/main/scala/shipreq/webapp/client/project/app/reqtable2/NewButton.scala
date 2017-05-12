package shipreq.webapp.client.project.app.reqtable2

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{Live, ReqTypeId, ReqTypes}
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.base.ui.semantic.{Dropdown, Icon, JQuery, UsesSemanticUiManually}
import shipreq.webapp.client.project.app.Style.reqtable2.{creation => *}

object NewButton {

  type State = Option[Choice]

  @inline def initState: State = None

  /**
    * @param update `None` means it has already been pressed and the creation-form is open, thus this is disabled.
    */
  final case class Props(state   : State,
                         reqTypes: ReqTypes,
                         allowRCG: Permission,
                         update  : Option[Reusable[Update]]) {
    @inline def render: VdomElement = Component(this)

    private[NewButton] lazy val dropdownItems: List[DropdownItem] = {
      var unsorted: List[DropdownItem] =
        reqTypes.all
          .iterator
          .filter(_.live is Live)
          .map(rt => DropdownItem(Some(rt.mnemonic), rt.name, Choice.Req(rt.reqTypeId), rt.mnemonic.value))
          .toList

      if (allowRCG is Allow)
        unsorted ::= DropdownItem(None, UiText.reqCodeGroup, Choice.ReqCodeGroup, "rcg")

      unsorted.sortBy(i => (i.mnemonic.fold("a")(_.value), i.name))
    }

  }

  sealed abstract class Choice
  object Choice {
    case class Req(id: ReqTypeId) extends Choice
    case object ReqCodeGroup extends Choice
    implicit def univEq: UnivEq[Choice] = UnivEq.derive
    implicit def reusability: Reusability[Choice] = Reusability.byUnivEq
  }

  final case class Update(setState: State => Callback, create: Choice => Callback)

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClass

  private case class DropdownItem(mnemonic: Option[Mnemonic],
                                  name    : String,
                                  choice  : Choice,
                                  key     : String)

  final class Backend($: BackendScope[Props, Unit]) {
    var dropdownNode: html.Element = null

    @UsesSemanticUiManually
    def render(p: Props): VdomElement = {

      val dropdownItems = p.dropdownItems

      val selected: DropdownItem =
        p.state
          .flatMap(c => dropdownItems.find(_.choice ==* c))
          .getOrElse(dropdownItems.head)

      def renderButton: VdomTag =
        <.a(
          ^.cls := "ui button green",
          Icon.Plus.tag,
          "New",
          ^.onClick -->? p.update.map(_.create(selected.choice)))

      def renderDropdown: VdomTag =
        <.div.ref(dropdownNode = _)(
          ^.cls := "ui dropdown label",
          *.buttonDropdown,
          selected.mnemonic.fold(selected.name)(_.value),
          Icon.Dropdown.tag,
          <.div(^.cls := "menu", renderDropdownItems))

      def renderDropdownItems: VdomArray =
        dropdownItems.toVdomArray(i =>
          <.div(
            ^.cls := "item",
            ^.key := i.key,
            Dropdown.itemValue := i.key,
            (^.cls := "active selected").when(selected.choice ==* i.choice),
            i.mnemonic.fold("")(_.value + ": ") + i.name))

      <.div(
        ^.cls := "ui labeled button",
        (^.cls := "disabled").when(p.update.isEmpty),
        renderButton,
        renderDropdown)
    }

    def selectChoice(value: String): Callback =
      (for {
        p <- $.props.toCBO
        u <- CallbackOption.liftOption(p.update)
        i <- CallbackOption.liftOption(p.dropdownItems.find(_.key ==* value))
        _ <- u.setState(Some(i.choice)).toCBO
      } yield ()).get.void

    private val dropdownOptions: Dropdown.JsOptions =
      new Dropdown.JsOptions {
        override val onChange = (selectChoice(_).runNow()): Dropdown.JsOptions.OnChange
      }

    val enableDropdown: Callback =
      Callback {
        JQuery(dropdownNode).dropdown(dropdownOptions)
      }
  }

  val Component = ScalaComponent.builder[Props]("NewButton")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}