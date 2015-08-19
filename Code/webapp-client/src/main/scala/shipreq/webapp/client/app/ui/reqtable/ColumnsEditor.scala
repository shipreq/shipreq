package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.Callback
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.data.CustomField
import shipreq.webapp.client.app.ui.OrderedSubsetEditor
import shipreq.webapp.client.app.ui.Style
import shipreq.webapp.client.lib.FilterDead

object ColumnsEditor {
  val Component = OrderedSubsetEditor.Component[Column]
}

final class ColumnsEditor(columnName: Column.NameResolver) {

  val allColumns: FilterDead => Vector[Column] =
    FilterDead.memo { fd =>
      val f      = fd.filterFnA[CustomField](_.live)
      val fields = columnName.customFields.values.toStream filter f
      Column.all(fields).whole
    }

  def render(filterDead: FilterDead, selected: NonEmptyVector[Column], update: NonEmptyVector[Column] => Callback) = {

    val update2: Vector[Column] => Callback =
      v => NonEmptyVector.maybe(v, Callback.empty)(update)

    val p = OrderedSubsetEditor.Props[Column](value     = selected.whole,
                                              all       = allColumns(filterDead),
                                              label     = columnName.fn,
                                              mandatory = Column.mandatory,
                                              change    = update2,
                                              styles    = (c: Column, o) => Style.reqtable.columnsEditor(c.live)(o))
    ColumnsEditor.Component(p)
  }
}
