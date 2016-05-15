package shipreq.webapp.client.project.app.cfg.shared

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.data.{Live, Dead, DataIdAux, FilterDead}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.event.{Delete, Restore}
import shipreq.webapp.client.project.widgets.Checkbox

// TODO So many c.state.runNow()s in CfgTable
object CfgTable {
  def apply[S, K <: TaggedInt, P, I, A, B, C, V](editor    : Editor[A, B, CallbackTo, S, C, Callback, V],
                                                 savedStore: SavedRowStore[S, K, P, I],
                                                 newStore  : NewRowStore[S, I])(implicit I: DataIdAux[P, K]) =
    new Tmp[S, K, P, I, A, B, C, V](editor, savedStore, newStore)(I)

  final class Tmp[S, K <: TaggedInt, P, I, A, B, C, V](editor    : Editor[A, B, CallbackTo, S, C, Callback, V],
                                                 savedStore: SavedRowStore[S, K, P, I],
                                                 newStore  : NewRowStore[S, I])(implicit I: DataIdAux[P, K]) {
      @inline def build[RowKey, N](rowkey      : P => RowKey,
                                   rr          : RowRenderer[P, V, N],
                                   newRowA     : I => A,
                                   savedRowA   : K => A,
                                   del         : () => Deletion[K],
                                   live        : P => Live,
                                   filterDeadCB: CallbackTo[FilterDead],
                                   c           : CompState.Access[S])
                                  (implicit O  : Ordering[RowKey]): CfgTable[S, K, P, I, A, B, C, V, RowKey, N] =
        new CfgTable(editor, savedStore, newStore, rowkey, rr, newRowA, savedRowA, del, live, filterDeadCB, c)
    }

  def typical[P, I, K <: TaggedInt](sas: TypicalStoresAndState[P, I, K], filterDeadCB: CallbackTo[FilterDead]) = new {
    type A = ((Stream[P], Option[K]), I)
    def apply[B, C, V](editor: Editor[A, B, CallbackTo, sas.S, C, Callback, V]) = new {
      def apply[RowKey, N](rowkey: P => RowKey,
                           rr: RowRenderer[P, V, N],
                           del: () => Deletion[K],
                           live: P => Live,
                           c: CompState.Access[sas.S])
                          (implicit I: DataIdAux[P, K], O: Ordering[RowKey])
      : CfgTable[sas.S, K, P, I, A, B, C, V, RowKey, N] = {
          def rowA(k: Option[K], i: I): editor.InputA = (sas.validatorInput(k)(c.state.runNow()), i)
          def newRowA  (i: I)  = rowA(None, i)
          def savedRowA(id: K) = rowA(Some(id), sas.savedRowStoreS.getI(id)(c.state.runNow()))
          new CfgTable(editor, sas.savedRowStoreS, sas.newRowStoreS, rowkey, rr, newRowA, savedRowA, del, live, filterDeadCB, c)
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
    def render    : R => Seq[TagMod]
  }
}

final class CfgTable[S, K <: TaggedInt, P, I, A, B, C, V, RowKey, R](editor      : Editor[A, B, CallbackTo, S, C, Callback, V],
                                                                     savedStore  : SavedRowStore[S, K, P, I],
                                                                     newStore    : NewRowStore[S, I],
                                                                     rowkey      : P => RowKey,
                                                                     rr          : CfgTable.RowRenderer[P, V, R],
                                                                     newRowA     : I => A,
                                                                     savedRowA   : K => A,
                                                                     deletion    : () => Deletion[K],
                                                                     live        : P => Live,
                                                                     filterDeadCB: CallbackTo[FilterDead],
                                                                     c           : CompState.Access[S])
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
      ^.disabled := newStore.editing(c.state.runNow()),
      "New")

  def newCancelButton: ReactElement =
    <.button(
      ^.onClick --> run(newStore.remove),
      "Cancel")

  def row(classArg: String, rs: RowStatus, content: RowContent, ctrls: => TagMod): ReactTag = {
    val cls2 = rowStatusRowClass(rs)
    val c = rowStatusCtrls(rs, ctrls)
    <.tr(
      ^.cls := s"$classArg $cls2",
      rr.render(content).map(<.td(_)),
      <.td(c))
  }

  def newRowO: Option[ReactTag] =
    newStore.get(c.state.runNow()).map(r => {
      val v = renderRow(newRowA(r.i), r.status)
      row("new", r.status, rr.newRow(v), newCancelButton)(^.key := "new")
    })

  def newRow: TagMod =
    newRowO.getOrElse(EmptyTag)

  def savedRows: RowStream = {
    val state = c.state.runNow()
    val filterDead = filterDeadCB.runNow()
    val del = deletion()
    var rs = savedStore.getAll(state)
    rs = filterDead(rs)(rowLive)
    rs.map(r => {
      val el = live(r.p) match {
        case Live => savedLiveRow(r.status, r.p, del)
        case Dead => savedDeadRow(r.status, r.p, del)
      }
      (rowkey(r.p), el)
    })
  }

  private def savedLiveRow(rs: RowStatus, p: P, d: Deletion[K]): ReactElement = {
    def del = d.button(p.id, Delete)
    val v = renderRow(savedRowA(p.id), rs)
    row("live", rs, rr.savedRow(v, p), del)(^.key := p.id.value)
  }

  private def savedDeadRow(rs: RowStatus, p: P, d: Deletion[K]): ReactElement = {
    def restore = d.button(p.id, Restore)
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

  def wrapWithFilterDeadCheckbox(set: FilterDead => Callback): ReactElement => ReactElement = {
    val checkbox = Checkbox.filterDead(ReusableFn(set))
    inner => <.div(checkbox(filterDeadCB.runNow()), inner)
  }
}