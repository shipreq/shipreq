package shipreq.webapp.client.app.ui.reqtable

import scalaz.effect.IO
import shipreq.base.util.IMap
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.OrderedSubsetEditor

object ColumnEditor {
  val Component = OrderedSubsetEditor.Component[Column]
}

final class ColumnEditor(customFields: IMap[CustomField.Id, CustomField], customFieldName: CustomField => String) {

  val allColumns: Vector[Column] =
    customFields.keys.toVector.map(Column.CustomField) ++ Column.nonFieldValues.list

  val labelLookup: Column => String = {
    case Column.CustomField(id) => customFields.get(id).map(customFieldName) getOrElse "?"
    case Column.ReqType         => ColumnNames.reqType
    case Column.PubId           => ColumnNames.pubId
    case Column.Code            => ColumnNames.code
    case Column.Desc            => ColumnNames.desc
  }

  def render(_value: Vector[Column], _change: Vector[Column] => IO[Unit]) = {
    val p = OrderedSubsetEditor.Props[Column](value     = _value,
                                              all       = allColumns,
                                              label     = labelLookup,
                                              mandatory = Column.mandatory,
                                              change    = _change)
    ColumnEditor.Component(p)
  }
}
