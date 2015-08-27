package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.Callback
import shipreq.webapp.base.data.FieldSet
import shipreq.webapp.client.app.ui.{OrderedSubsetEditor, Style}

object ColumnsEditor {
  val OSE = new OrderedSubsetEditor[Column]
  val State = OSE.State
  type State = OSE.State

  def apply(state       : State,
            update      : State => Callback,
            columnNames : Column.NameResolver,
            customFields: FieldSet.CustomFields,
            filterFn    : Column => Boolean) = {

    val p = OSE.Props(state, update,
                      label     = columnNames.fn,
                      mandatory = Column.mandatory,
                      filter    = filterFn,
                      styles    = (c: Column, o) => Style.reqtable.columnsEditor(c.live)(o))
    OSE.Component(p)
  }
}
