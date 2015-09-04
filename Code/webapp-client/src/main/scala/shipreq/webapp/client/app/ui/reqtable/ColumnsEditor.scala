package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.base.util.NonEmptySet
import shipreq.webapp.client.app.ui.CheckboxList
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.util.{Disabled, On}

object ColumnsEditor {

  val checkboxList =
    new CheckboxList[Column](checkboxes =>
      <.div(
        checkboxes map (<.div(_))))

  case class Props(on          : NonEmptySet[Column],
                   toggle      : Column ~=> Callback,
                   columnNames : Column.NameResolver,
                   all         : NonEmptySet[Column])

  implicit val reusabilityCols = reusabilityNonEmptySet[Column]
  implicit val reusabilityProps = Reusability.caseClass[Props]

  private def render(p: Props) = {
    val items =
      p.all.toStream
        .map(c => CheckboxList.Item(c, p.columnNames(c), On <~ p.on.contains(c), Disabled <~ Column.isMandatory(c)))
        .sortBy(_.label)
        .toVector

    val p2 = CheckboxList.Props(items, p.toggle)
    checkboxList.Component(p2)
  }

  val Component = ReactComponentB[Props]("ColumnsEditor")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
