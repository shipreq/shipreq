package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import shipreq.webapp.base.data._
import shipreq.webapp.client.util.EVar

object ReqTable {

  val WIP =
    ReactComponentB[Project]("WIP")
      .initialState(ViewSettings.default)
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  final class Backend($: BackendScope[Project, ViewSettings]) {

    val project = $.props // TODO make Refreshable

    val columnName =
      Column.NameResolver(project.fields.data.customFields, CustomField.nameP(project))

    val viewSettingsEditor =
      ViewSettingsEditor(columnName)

    def render =
      viewSettingsEditor(EVar overState $)
  }
}
