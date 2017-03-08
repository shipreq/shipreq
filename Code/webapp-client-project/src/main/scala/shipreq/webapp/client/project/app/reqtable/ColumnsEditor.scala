package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.microlibs.nonempty.NonEmptySet
import shipreq.webapp.client.base.data.{Enabled, On}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.CheckboxList

object ColumnsEditor {

  val checkboxList =
    new CheckboxList[Column](checkboxes =>
      <.div(
        checkboxes.toTagMod(<.div(_))))

  case class Props(on          : NonEmptySet[Column],
                   toggle      : Column ~=> Callback,
                   columnNames : Column.NameResolver,
                   all         : NonEmptySet[Column])

  implicit val reusabilityCols = reusabilityNonEmptySet[Column]
  implicit val reusabilityProps = Reusability.caseClass[Props]

  private def render(p: Props) = {
    val items =
      p.all.iterator
        .filter(!Column.isMandatory(_))
        .map(c => CheckboxList.Item(c, p.columnNames(c), On <~ p.on.contains(c), Enabled))
        .toVector
        .sortBy(_.label)

    val p2 = CheckboxList.Props(items, p.toggle)
    checkboxList.Component(p2)
  }

  val Component = ScalaComponent.build[Props]("ColumnsEditor")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
