package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{Live, ReqTypes}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.semantic.{Dropdown, Icon, JQuery, UsesSemanticUiManually}
import shipreq.webapp.base.ui.semantic.Dropdown.JsOptionsOps
import shipreq.webapp.client.project.app.Style.reqtable.{creation => *}
import shipreq.webapp.client.project.feature.CreateFeature.RowKey

object NewButton {

  type State = Option[RowKey]

  @inline def initState: State = None

  /**
    * @param update `None` means this is disabled.
    */
  final case class Props(state   : State,
                         reqTypes: ReqTypes,
                         allowRCG: Permission,
                         default : Option[RowKey],
                         update  : Option[Reusable[Update]]) {
    @inline def render: VdomElement = Component(this)

    private[NewButton] val dropdownItems: List[DropdownItem] = {
      var unsorted: List[DropdownItem] =
        reqTypes.all
          .iterator
          .filter(_.live is Live)
          .map(rt => DropdownItem(Some(rt.mnemonic), rt.name, RowKey.req(rt.reqTypeId), rt.mnemonic.value))
          .toList

      if (allowRCG is Allow)
        unsorted ::= DropdownItem(None, UiText.codeGroup, RowKey.CodeGroup, ".cg")

      unsorted.sortBy(i => (i.mnemonic.fold("a")(_.value), i.name))
    }

    private[NewButton] val selectedDropdownItem: Option[DropdownItem] =
      state.flatMap(c => dropdownItems.find(_.value ==* c))

    val selected: Option[RowKey] =
      selectedDropdownItem.map(_.value)
  }

  final case class Update(setState: RowKey => Callback, create: RowKey => Callback)

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private case class DropdownItem(mnemonic: Option[Mnemonic],
                                  name    : String,
                                  value   : RowKey,
                                  key     : String)

  final class Backend($: BackendScope[Props, Unit]) {
    private val dropdownNode = Ref[html.Element]

    @UsesSemanticUiManually
    def render(p: Props): VdomElement = {

      val dropdownItems = p.dropdownItems

      val selected: DropdownItem =
        p.selectedDropdownItem
          .orElse(p.default.flatMap(k => dropdownItems.find(_.value ==* k)))
          .getOrElse(dropdownItems.head)

      def renderButton: VdomTag =
        <.a(
          ^.cls := "ui button green",
          Icon.Plus.tag,
          "New",
          ^.onClick -->? p.update.map(_.create(selected.value)))

      def renderDropdown: VdomTag =
        <.div.withRef(dropdownNode)(
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
            (^.cls := "active selected").when(selected.value ==* i.value),
            i.mnemonic.fold("")(_.value + ": ") + i.name))

      <.div(
        *.buttonOuter,
        ^.cls := "ui labeled button",
        (^.cls := "disabled").when(p.update.isEmpty),
        renderButton,
        renderDropdown)
    }

    def selectChoice(value: String): Callback =
      for {
        p <- $.props.toCBO
        u <- CallbackOption.liftOption(p.update)
        i <- CallbackOption.liftOption(p.dropdownItems.find(_.key ==* value))
        _ <- u.setState(i.value).toCBO
      } yield ()

    private val dropdownOptions: Dropdown.JsOptions =
      Dropdown.JsOptions.default.withOnChange(selectChoice(_).runNow())

    val enableDropdown: Callback =
      dropdownNode.foreach(JQuery(_).dropdown(dropdownOptions))
  }

  val Component = ScalaComponent.builder[Props]("NewButton")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}