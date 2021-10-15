package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ui.semantic.Header
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.widgets._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.util.DataReusability._

private[reqdetail] object HeaderRow {

  final case class Props(pubidText   : String,
                         live        : Live,
                         titleEditor : EditorFeature.ReadWrite.ForEditor[Unit, Any],
                         titleView   : Reusable[VdomNode],
                         filterDead  : StateSnapshot[FilterDead],
                         tableRef    : Ref.Simple[html.Table],
                         titleCellRef: Ref.Simple[html.Element],
                        ) {
    @inline def render: VdomElement = Component.withKey("H")(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClassExcept("tableRef", "titleCellRef")

  private val headerStyle: Live => Header.Style =
    Live.memo(l => Header.Style(Header.Type.H1, other = *.headerText(l)))

  final class Backend($: BackendScope[Props, Unit]) {

    private val titleCellOnKeyDown: ReactKeyboardEventFromHtml => CallbackOption[Unit] = {
      def focusRow(top: Boolean) =
        for {
          p     <- $.props.toCBO
          table <- p.tableRef.get.asCBO
        } yield {
          val tbody = table.children(0)
          val tr    = tbody.children(if (top) 0 else tbody.children.length - 1)
          val td    = tr.children(1).domAsHtml
          td.focus()
        }

      e =>
        (CallbackOption.keyCodeSwitch(e) {
          case KeyCode.Up   => focusRow(top = false)
          case KeyCode.Down => focusRow(top = true)
        } | CallbackOption.keyCodeSwitch(e, ctrlKey = true) {
          case KeyCode.End
             | KeyCode.Down => focusRow(top = false)
        }).asEventDefault(e)
    }

    private val titleCellBase =
      <.div(*.headerTitle, ^.tabIndex := -1)

    private val focusTitle: Callback =
      $.props.flatMap(_.titleCellRef.get.asCBO.map(_.focus()))

    def render(p: Props): VdomNode = {
      val hstyle      = headerStyle(p.live)
      val titleEditor = p.titleEditor.onClose(focusTitle)

      val onTitleKeyDown: ReactKeyboardEventFromHtml => Callback =
        e => titleCellOnKeyDown(e) | EditorFeature.Keys(titleEditor)(e)

      val titleCell =
        titleCellBase.withRef(p.titleCellRef)(
          ^.onKeyDown ==> onTitleKeyDown,
          titleEditor.themedRenderOr(())(Header(hstyle, p.titleView.value)))

      <.div(*.headerRow,
        <.div(*.headerPubid, Header(hstyle, p.pubidText + ":")),
        titleCell,
        <.div(*.headerFilterDeadButton,
          FilterDeadButton.whenLive(p.live)(p.filterDead)))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
