package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import scalaz.Memo
import scalaz.effect.IO
import shipreq.base.util.ScalaExt._
import shipreq.base.util.Rx
import shipreq.webapp.base.data._

final class ColumnEditors(project  : Rx[Project],
                          reqDescFn: Rx[Req => String],
                          cellset  : Cell.SetIO) {

  type SetLocal = Option[Cell.State] => IO[Unit]

  type ColStartEdit = (Row, SetLocal) => Option[Cell.State]

  def startCellEditing(row: Row, col: Column): Option[IO[Unit]] = {
    val e: ColStartEdit =
      col match {
        case Column.Tags           => tags
        case Column.Pubid          => noEdit
        case Column.ImplicationSrc => imps(Row.implicationSrc)
        case Column.ImplicationTgt => imps(Row.implicationTgt)
        case Column.CustomField(f) =>
          f match {
            // case id: CustomField.Text       .Id => cfText(id)
            case id: CustomField.Tag        .Id => cfTag(id)
            case id: CustomField.Implication.Id => cfImp(id)
          }
      }

    val setLocal: SetLocal =
      s => cellset(Cell.SetCmd(row.id, col, s))

    val startState = e(row, setLocal)

    startState.map(_ => setLocal(startState))
  }

  val noEdit: ColStartEdit =
    (_, _) => None

  lazy val tags: ColStartEdit = {
    val lookup = TagEditor.lookupForNoCol(project)
    (row, setLocal) => {
      val initial = row.fold(_.mv.tags)
      TagEditor(initial, project.value(), lookup, setLocal).some
    }
  }

  val cfTag: CustomField.Tag.Id => ColStartEdit =
    Memo.mutableHashMapMemo { id =>
      val lookup = TagEditor.lookupForCol(project, id)
      (row, setLocal) => {
        val initial = row.fold(_.exp.cfTags.getOrElse(id, Vector.empty))
        TagEditor(initial, project.value(), lookup, setLocal).some
      }
    }

  lazy val impsLookup = ImplicationEditor.lookupAll(project, reqDescFn)

  def imps(l: Optional[Row, Vector[Pubid]]): ColStartEdit = (row, setLocal) =>
    l.getOption(row).map(
      ImplicationEditor(_, project.value(), impsLookup, setLocal))

  def cfImp(id: CustomField.Implication.Id): ColStartEdit = (row, setLocal) =>
    Row.cfImp(id).getOption(row).map { initial =>
      val lookup = ImplicationEditor.lookupForCol(project, impsLookup, id)
      ImplicationEditor(initial, project.value(), lookup, setLocal)
    }

}
