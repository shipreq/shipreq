package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.univeq._
import shipreq.webapp.client.base.data._
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.base.ui.semantic.{Button, Icon, Popup}
import shipreq.webapp.client.project.widgets.CheckboxList

/**
  * Shows a little column button:
  *
  *   [=]
  *
  * Which the user can then click on to popup a list of columns they can toggle
  *
  *   [=] +------------+
  *       | [x] Code   |
  *       | [ ] Status |
  *       | [x] Title  |
  *       +------------+
  */
object ColumnSelector {

  final case class Props(active   : NonEmptyVector[Column],
                         available: ColumnPlus.All,
                         update   : NonEmptyVector[Column] ~=> Callback) {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClass

  private val popupOptions: Popup.Js.Options =
    new Popup.Js.Options {
      override val inline = true
      override val hoverable = true
      override val position = Popup.Position.LeftCenter.value
      override val delay = new Popup.Js.Options.Delay {
        override val show = 200
        override val hide = 300
      }
    }

  private val button = Button(
    tipe = Button.Type.IconOnly(Icon.Columns),
    state = Button.State.Active)

  private val ColumnCheckboxes = CheckboxList[Column] (rs =>
    Popup.Js.Props(
      popupOptions,
      button.tag,
      rs.toTagMod(r => Popup.renderCheckbox(r.checkbox, r.label)))
      .render)

  private def render(p: Props): VdomElement = {

    val activeColumns: Set[Column] =
      p.active.whole.toSet

    val unselectedColumns: Array[ColumnPlus] =
      p.available.columns
        .iterator
        .filter(c => !activeColumns.contains(c.column))
        .toArray
        .sortBy(_.name)

    val columns: NonEmptyVector[(ColumnPlus, On)] =
      p.active.map(c => (p.available(c), On)) ++ unselectedColumns.map((_, Off))

    val items: NonEmptyVector[ColumnCheckboxes.Item] =
      columns.map { case (c, on) =>
        ColumnCheckboxes.Item(c.column, c.name, on, Disabled when Column.isMandatory(c.column))
      }

    ColumnCheckboxes.Props(
      items,
      p.update.map(set => update => set(NonEmptyVector force update.newSelection)))
      .render
  }

  val Component = ScalaComponent.builder[Props]("ColumnSelector")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
