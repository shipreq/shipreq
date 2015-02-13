package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.webapp.base.data._

object ReqTable {

  val WIP =
    ReactComponentB[Project]("WIP")
      .initialState[Vector[Column]](Column.nonFieldValues.list.toVector)
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  final class Backend($: BackendScope[Project, Vector[Column]]) {

    val ed = new ColumnEditor($.props.fields.data.customFields, CustomField.nameP($.props))

    def render = ed.render($.state, $.setStateIO(_))
  }
}
