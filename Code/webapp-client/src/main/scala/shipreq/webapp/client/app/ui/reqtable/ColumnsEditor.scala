package shipreq.webapp.client.app.ui.reqtable

import scalaz.effect.IO
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.client.app.ui.OrderedSubsetEditor
import shipreq.webapp.client.app.ui.Style

object ColumnsEditor {
  val Component = OrderedSubsetEditor.Component[Column]
}

final class ColumnsEditor(columnName: Column.NameResolver) {

  val allColumns: Vector[Column] =
    Column.all(columnName.customFields.keys).whole

  def render(_value: NonEmptyVector[Column], _change: NonEmptyVector[Column] => IO[Unit]) = {
    val _change2: Vector[Column] => IO[Unit] =
      v => NonEmptyVector.maybe(v, IO(()))(_change)
    val p = OrderedSubsetEditor.Props[Column](value     = _value.whole,
                                              all       = allColumns,
                                              label     = columnName.fn,
                                              mandatory = Column.mandatory,
                                              change    = _change2,
                                              styles    = Style.reqtable.columnsEditor)
    ColumnsEditor.Component(p)
  }
}
