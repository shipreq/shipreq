package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<*._, ScalazReact._
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.DeletionAction._
import DataImplicits._
import scalaz.effect.IO

object CfgTable {
  def apply[S, K <: TaggedLong, P, I, A, B, C, V](editor: Editor[A, B, IO, S, C, IO[Unit], V],
                                                  savedStore: SavedRowStore[S, K, P, I],
                                                  newStore: NewRowStore[S, I])(implicit I: DataIdAux[P, K]) =
    new {
      @inline def build[RowKey, N](rowkey: P => RowKey,
                                   rr: RowRenderer[P, V, N],
                                   newRowA: I => A,
                                   savedRowA: K => A,
                                   del: Deletion[P, K],
                                   showDeleted: S => Boolean,
                                   c: ComponentStateFocus[S])
                                  (implicit O: Ordering[RowKey]): CfgTable[S, K, P, I, A, B, C, V, RowKey, N] =
        new CfgTable(editor, savedStore, newStore, rowkey, rr, newRowA, savedRowA, del, showDeleted, c)
    }

  def typical[P, I, K <: TaggedLong](sas: TypicalStoresAndState[P, I, K]) = new {
    type A = ((Stream[P], Option[K]), I)
    def apply[B, C, V](editor: Editor[A, B, IO, sas.S, C, IO[Unit], V]) = new {
      def apply[RowKey, N](rowkey: P => RowKey,
                           rr: RowRenderer[P, V, N],
                           del: Deletion[P, K],
                           c: ComponentStateFocus[sas.S])
                          (implicit I: DataIdAux[P, K], O: Ordering[RowKey])
      : CfgTable[sas.S, K, P, I, A, B, C, V, RowKey, N] = {
          def rowA(k: Option[K], i: I): editor.InputA = (sas.validatorInput(k)(c.state), i)
          def newRowA  (i: I)  = rowA(None, i)
          def savedRowA(id: K) = rowA(Some(id), sas.savedRowStoreS.getI(id)(c.state))
          new CfgTable(editor, sas.savedRowStoreS, sas.newRowStoreS, rowkey, rr, newRowA, savedRowA, del, _.showDeleted, c)
        }
      }
    }

  def header(headers: List[String]): ReactElement =
    <.thead(<.tr(headers.map(<.th(_)), <.th("Ctrls")))

  /**
   * @tparam P Persisted data. Data known to be saved.
   * @tparam I Input. A subset of P's fields in a form that matches the editor state.
   * @tparam R Row content prior to being rendered into DOM.
   */
  trait RowRenderer[P, I, R] {
    def newRow    : I => R
    def savedRow  : (I, P) => R
    def deletedRow: P => R
    def render    : R => Seq[Modifier]
  }
}

final class CfgTable[S, K <: TaggedLong, P, I, A, B, C, V, RowKey, R](editor: Editor[A, B, IO, S, C, IO[Unit], V],
                                                                      savedStore: SavedRowStore[S, K, P, I],
                                                                      newStore: NewRowStore[S, I],
                                                                      rowkey: P => RowKey,
                                                                      rr: CfgTable.RowRenderer[P, V, R],
                                                                      newRowA: I => A,
                                                                      savedRowA: K => A,
                                                                      deletion: Deletion[P, K],
                                                                      showDeleted: S => Boolean,
                                                                      c: ComponentStateFocus[S])
                                                                     (implicit I: DataIdAux[P, K], O: Ordering[RowKey]) {
  /** Row content prior to being rendered into DOM. */
  type RowContent = R
  type RowStream = Stream[(RowKey, ReactElement)]

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

  def row(classArg: String, rs: RowStatus, content: RowContent, ctrls: => Modifier): Tag = {
    val cls2 = UiLib.rowStatusRowClass(rs)
    val c = UiLib.rowStatusCtrls(rs, ctrls)
    <.tr(
      *.cls := s"$classArg $cls2",
      rr.render(content).map(<.td(_)),
      <.td(c))
  }

  def newRowO: Option[Tag] =
    newStore.get(c.state).map(r => {
      val v = renderRow(newRowA(r.i), r.status)
      row("new", r.status, rr.newRow(v), newCancelButton)(*.keyAttr := "new")
    })

  def newRow: Modifier =
    newRowO.getOrElse(EmptyTag)

  def savedRows: RowStream = {
    var rs = savedStore.getAll(c.state)
    if (!showDeleted(c.state))
      rs = rs.filter(r => deletion.filterAlive(r.p))
    rs.map(r => {
      val el = deletion.alive(r.p) match {
        case Alive => savedLiveRow(r.status, r.p)
        case Dead  => savedDeadRow(r.status, r.p)
      }
      (rowkey(r.p), el)
    })
  }

  private def savedLiveRow(rs: RowStatus, p: P): ReactElement = {
    def del1 = deletion.button(p.id, HardDel)
    def del2 = deletion.button(p.id, SoftDel)
    val v = renderRow(savedRowA(p.id), rs)
    row("live", rs, rr.savedRow(v, p), <.span(del1, del2))(*.keyAttr := p.id.value)
  }

  private def savedDeadRow(rs: RowStatus, p: P): ReactElement = {
    def restore = deletion.button(p.id, Restore)
    row("dead", rs, rr.deletedRow(p), restore)(*.keyAttr := p.id.value)
  }

  def allSortableRows(static: RowStream) =
    (static #::: savedRows).sortBy(_._1).map(_._2).toReactNodeArray

  def table(header: ReactElement, static: RowStream): ReactElement =
    <.div(
      newButton,
      <.table(
        header,
        <.tbody(newRow, allSortableRows(static))))
}