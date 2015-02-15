package shipreq.webapp.client.app.ui.reqtable

import scalaz.effect.IO
import shipreq.webapp.client.app.ui.OrderedSubsetEditor

object ColumnsEditor {
  val Component = OrderedSubsetEditor.Component[Column]
}

final class ColumnsEditor(columnName: Column.NameResolver) {

  val allColumns: Vector[Column] =
    columnName.customFields.keys.toVector.map(Column.CustomField) ++ Column.nonFieldValues.list

  def render(_value: Vector[Column], _change: Vector[Column] => IO[Unit]) = {
    val p = OrderedSubsetEditor.Props[Column](value     = _value,
                                              all       = allColumns,
                                              label     = columnName.fn,
                                              mandatory = Column.mandatory,
                                              change    = _change)
    ColumnsEditor.Component(p)
  }
}
