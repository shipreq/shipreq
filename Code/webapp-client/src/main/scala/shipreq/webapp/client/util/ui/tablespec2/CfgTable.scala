package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.prefix_<*._, ScalazReact._
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.DeletionAction
import shipreq.webapp.base.protocol.DeletionAction._
//import shipreq.webapp.client.lib.UiLib
//import shipreq.webapp.client.util.ui.Util
import DataImplicits._
import scalaz.effect.IO


trait CfgTableCells[P, A, Norm] {
  def newRow    : A => Norm
  def savedRow  : (A, P) => Norm
  def deletedRow: P => Norm
  def render    : Norm => Seq[Modifier]
}

object CfgTable {
  def apply[S, K <: TaggedLong, P, I, A, B, C, V](editor: Editor[A, B, IO, S, C, IO[Unit], V],
                                                  savedStore: SavedRowStore[S, K, P, I],
                                                  newStore: NewRowStore[S, I])(implicit I: DataIdAux[P, K]) = new {
    def then[RowKey,N](rowkey: P => RowKey,
                     cellfmt: CfgTableCells[P, V, N],
                     newRowA: I => A,
                     savedRowA: K => A,
                     del: Deletion[P, K],
                     showDeleted: S => Boolean,
                     c: ComponentStateFocus[S])
                    (implicit O: Ordering[RowKey]): CfgTable[S, K, P, I, A, B, C, V, RowKey, N] =
      new CfgTable(editor, savedStore, newStore, rowkey, cellfmt, newRowA, savedRowA, del, showDeleted, c)
  }
}

final class CfgTable[S, K <: TaggedLong, P, I, A, B, C, V, RowKey, N](editor: Editor[A, B, IO, S, C, IO[Unit], V],
                                                                   savedStore: SavedRowStore[S, K, P, I],
                                                                   newStore: NewRowStore[S, I],
                                                                   rowkey: P => RowKey,
                                                                   cellfmt: CfgTableCells[P, V, N],
                                                                   newRowA: I => A,
                                                                   savedRowA: K => A,
                                                                   del: Deletion[P, K],
                                                                   showDeleted: S => Boolean,
                                                                   c: ComponentStateFocus[S])
                                                                  (implicit I: DataIdAux[P, K], O: Ordering[RowKey]) {
  type RowStream = Stream[(RowKey, ReactElement)]
  type Norm = N

  private[this] val ST = ReactS.FixT[IO, S]
  private[this] def run(s: ST.T[Unit]): IO[Unit] = c.runState(s)
  private[this] implicit def endofToReactST(f: S => S) = ST modT f

  private[this] val editable: RowStatus => Option[editor.Editable] = {
    val canedit = editor.editable(c runState _.st)
    rs => rs match {
      case RowStatus.Sync | RowStatus.Failed(_) => canedit
      case RowStatus.Locked                     => None
    }
  }

  private[this] def renderRow(a: A, rs: RowStatus): V =
    editor render EditorI(a, "", editable(rs))

  def newButton: ReactElement =
    <.button(
      *.onclick ~~> run(newStore.enableEdit),
      *.disabled := newStore.editing(c.state),
      "New")

  def newCancelButton: ReactElement =
    <.button(
      *.onclick ~~> run(newStore.remove),
      "Cancel")

  def row(classArg: String, rs: RowStatus, cells: Norm, ctrls: => Modifier): Tag = {
    val cls2 = UiLib.rowStatusRowClass(rs)
    val c = UiLib.rowStatusCtrls(rs, ctrls)
    <.tr(
      *.cls := s"$classArg $cls2",
      cellfmt.render(cells).map(<.td(_)),
      <.td(c))
  }

  def newRowO: Option[Tag] =
    newStore.get(c.state).map(r => {
      val v = renderRow(newRowA(r.i), r.status)
      row("new", r.status, cellfmt.newRow(v), newCancelButton)(*.keyAttr := "new")
    })

  def newRow: Modifier = newRowO.getOrElse(EmptyTag)

  def savedRows: RowStream = {
    var rs = savedStore.rowStream(c.state)
    if (!showDeleted(c.state))
      rs = rs.filter(r => del.filterAlive(r.p))
    rs.map(r => {
      val el = del.alive(r.p) match {
        case Alive => liveSavedRow(r.status, r.p)
        case Dead  => deadSavedRow(r.status, r.p)
      }
      (rowkey(r.p), el)
    })
  }

  private def liveSavedRow(rs: RowStatus, p: P): ReactElement = {
    def del1 = del.button(p.id, HardDel)
    def del2 = del.button(p.id, SoftDel)
    val v = renderRow(savedRowA(p.id), rs)
    row("live", rs, cellfmt.savedRow(v, p), Seq(del1, del2).toReactNodeArray)(*.keyAttr := p.id.value)
  }

  private def deadSavedRow(rs: RowStatus, p: P): ReactElement = {
    def restore = del.button(p.id, Restore)
    row("dead", rs, cellfmt.deletedRow(p), restore)(*.keyAttr := p.id.value)
  }

  def allSortableRows(static: RowStream) =
    (static #::: savedRows).sortBy(_._1).map(_._2).toReactNodeArray

  def table(headers: List[String], static: RowStream) =
    <.div(
      newButton,
      <.table(
        <.thead(<.tr(headers.map(<.th(_)), <.th("Ctrls"))), // TODO bad perf
        <.tbody(newRow, allSortableRows(static))))
}