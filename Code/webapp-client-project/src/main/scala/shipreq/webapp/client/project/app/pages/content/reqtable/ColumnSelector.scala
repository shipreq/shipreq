package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.semantic.{Button, Icon, Popup}
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
    Reusability.derive

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
    tipe = Button.Type.IconOnly(Icon.Columns))

  private val ColumnCheckboxes = CheckboxList[Column] (rs =>
    Popup.Js.Props(
      popupOptions,
      button.tag,
      rs.toTagMod(r => Popup.renderCheckbox(r.checkbox, r.label)))
      .render)

  private def updateColumnList(active: NonEmptyVector[Column], clicked: Column, addClicked: Boolean): NonEmptyVector[Column] =
    NonEmptyVector force {

      val insertAfter: Option[Column.Mandatory] =
        clicked match {
          case Column.Code => Some(Column.Pubid)
          case _           => None
        }

      val b = Vector.newBuilder[Column]
      for (c <- active.whole) {
        if (c !=* clicked)
          b += c
        if (addClicked && insertAfter.exists(_ ==* c))
          b += clicked
      }

      // Append new items to the end by default
      if (addClicked && insertAfter.isEmpty)
        b += clicked

      b.result()
    }

  private def render(p: Props): VdomElement = {

    val activeColumns: Set[Column] =
      p.active.toNES.whole

    val columns: NonEmptyVector[(ColumnPlus, On)] =
      NonEmptyVector.force(
        MutableArray.map(p.available.columns.whole)(c => (c, On when activeColumns.contains(c.column)))
          .sortBy(_._1.name)
          .iterator
          .to(Vector))

    val items: NonEmptyVector[ColumnCheckboxes.Item] =
      columns.map { case (c, on) =>
        ColumnCheckboxes.Item(c.column, c.name, on, Disabled when Column.isMandatory(c.column))
      }

    val updateFn: ColumnCheckboxes.Update ~=> Callback =
      Reusable.ap(p.update, Reusable.implicitly(p.active))((set, active) =>
        update => set(updateColumnList(active, update.clickedItem.value, update.newValue is On)))

    ColumnCheckboxes.Props(items, updateFn).render
  }

  val Component = ScalaComponent.builder[Props]("ColumnSelector")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
