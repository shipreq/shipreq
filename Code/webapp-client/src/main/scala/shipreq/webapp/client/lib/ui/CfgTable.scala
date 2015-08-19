package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import monocle.Lens
import scalaz.syntax.equal._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.data.{Live, Dead, DataIdAux}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.event.{DeletionAction, HardDel, SoftDel, Restore}
import shipreq.webapp.client.lib.FilterDead

object CfgTable {
  def apply[S, K <: TaggedInt, P, I, A, B, C, V](editor: Editor[A, B, CallbackTo, S, C, Callback, V],
                                                  savedStore: SavedRowStore[S, K, P, I],
                                                  newStore: NewRowStore[S, I])(implicit I: DataIdAux[P, K]) =
    new {
      @inline def build[RowKey, N](rowkey: P => RowKey,
                                   rr: RowRenderer[P, V, N],
                                   newRowA: I => A,
                                   savedRowA: K => A,
                                   del: Deletion[K],
                                   live: P => Live,
                                   filterDead: S => FilterDead,
                                   c: CompStateFocus[S])
                                  (implicit O: Ordering[RowKey]): CfgTable[S, K, P, I, A, B, C, V, RowKey, N] =
        new CfgTable(editor, savedStore, newStore, rowkey, rr, newRowA, savedRowA, del, live, filterDead, c)
    }

  def typical[P, I, K <: TaggedInt](sas: TypicalStoresAndState[P, I, K]) = new {
    type A = ((Stream[P], Option[K]), I)
    def apply[B, C, V](editor: Editor[A, B, CallbackTo, sas.S, C, Callback, V]) = new {
      def apply[RowKey, N](rowkey: P => RowKey,
                           rr: RowRenderer[P, V, N],
                           del: Deletion[K],
                           live: P => Live,
                           c: CompStateFocus[sas.S])
                          (implicit I: DataIdAux[P, K], O: Ordering[RowKey])
      : CfgTable[sas.S, K, P, I, A, B, C, V, RowKey, N] = {
          def rowA(k: Option[K], i: I): editor.InputA = (sas.validatorInput(k)(c.state), i)
          def newRowA  (i: I)  = rowA(None, i)
          def savedRowA(id: K) = rowA(Some(id), sas.savedRowStoreS.getI(id)(c.state))
          new CfgTable(editor, sas.savedRowStoreS, sas.newRowStoreS, rowkey, rr, newRowA, savedRowA, del, live, _.filterDead, c)
        }
      }
    }

  def header(headers: List[String]): ReactElement =
    <.thead(<.tr(headers.map(<.th(_)), <.th("Ctrls")))


  def outer[P, I, K](sas: TypicalStoresAndState[P, I, K])(c: CompStateFocus[sas.S]): ReactElement => ReactElement = {
    val checkbox = sas.filterDeadCheckbox(c)
    <.div(checkbox(), _)
  }

  /**
   * @tparam P Persisted data. Data known to be saved.
   * @tparam I Input. A subset of P's fields in a form that matches the editor state.
   * @tparam R Row content prior to being rendered into DOM.
   */
  trait RowRenderer[P, I, R] {
    def newRow    : I => R
    def savedRow  : (I, P) => R
    def deletedRow: P => R
    def render    : R => Seq[TagMod]
  }
}

final class CfgTable[S, K <: TaggedInt, P, I, A, B, C, V, RowKey, R](editor: Editor[A, B, CallbackTo, S, C, Callback, V],
                                                                      savedStore: SavedRowStore[S, K, P, I],
                                                                      newStore: NewRowStore[S, I],
                                                                      rowkey: P => RowKey,
                                                                      rr: CfgTable.RowRenderer[P, V, R],
                                                                      newRowA: I => A,
                                                                      savedRowA: K => A,
                                                                      deletion: Deletion[K],
                                                                      live: P => Live,
                                                                      filterDead: S => FilterDead,
                                                                      c: CompStateFocus[S])
                                                                     (implicit I: DataIdAux[P, K], O: Ordering[RowKey]) {
  /** Row content prior to being rendered into DOM. */
  type RowContent = R
  type RowStream = Stream[(RowKey, ReactElement)]

  private[this] val ST = ReactS.FixCB[S]
  private[this] def run(s: ST.T[Unit]): Callback = c.runState(s)
  private[this] implicit def endofToReactST(f: S => S) = ST modT f

  private[this] val editable = editor.editableByRowStatus(c)

  private[this] val rowLive: savedStore.Row => Live =
    r => live(r.p)

  private[this] def renderRow(a: A, rs: RowStatus): V =
    editor render EditorI(a, "", editable(rs))

  def newButton: ReactElement =
    <.button(
      ^.onClick --> run(newStore.enableEdit),
      ^.disabled := newStore.editing(c.state),
      "New")

  def newCancelButton: ReactElement =
    <.button(
      ^.onClick --> run(newStore.remove),
      "Cancel")

  def row(classArg: String, rs: RowStatus, content: RowContent, ctrls: => TagMod): ReactTag = {
    val cls2 = UI.rowStatusRowClass(rs)
    val c = UI.rowStatusCtrls(rs, ctrls)
    <.tr(
      ^.cls := s"$classArg $cls2",
      rr.render(content).map(<.td(_)),
      <.td(c))
  }

  def newRowO: Option[ReactTag] =
    newStore.get(c.state).map(r => {
      val v = renderRow(newRowA(r.i), r.status)
      row("new", r.status, rr.newRow(v), newCancelButton)(^.key := "new")
    })

  def newRow: TagMod =
    newRowO.getOrElse(EmptyTag)

  def savedRows: RowStream = {
    val state = c.state
    var rs = savedStore.getAll(state)
    rs = filterDead(state)(rs)(rowLive)
    rs.map(r => {
      val el = live(r.p) match {
        case Live => savedLiveRow(r.status, r.p)
        case Dead => savedDeadRow(r.status, r.p)
      }
      (rowkey(r.p), el)
    })
  }

  private def savedLiveRow(rs: RowStatus, p: P): ReactElement = {
    def del1 = deletion.button(p.id, HardDel)
    def del2 = deletion.button(p.id, SoftDel)
    val v = renderRow(savedRowA(p.id), rs)
    row("live", rs, rr.savedRow(v, p), <.span(del1, del2))(^.key := p.id.value)
  }

  private def savedDeadRow(rs: RowStatus, p: P): ReactElement = {
    def restore = deletion.button(p.id, Restore)
    row("dead", rs, rr.deletedRow(p), restore)(^.key := p.id.value)
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