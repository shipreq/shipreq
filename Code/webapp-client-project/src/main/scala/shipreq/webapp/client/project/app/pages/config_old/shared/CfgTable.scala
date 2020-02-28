package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import vdom.html_<^._
import ScalazReact._
import japgolly.scalajs.react.extra._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.data.{DataIdAux, Dead, FilterDead, Live}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Table}
import shipreq.webapp.client.project.widgets.{CancelButton, FilterDeadButton}

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
                                   c           : StateAccessPure[S])
                                  (implicit O  : Ordering[RowKey]): CfgTable[S, K, P, I, A, B, C, V, RowKey, N] =
        new CfgTable(editor, savedStore, newStore, rowkey, rr, newRowA, savedRowA, del, live, filterDeadCB, c)
    }

  def header(headers: List[TagMod]): VdomElement =
    <.thead(<.tr(headers.toTagMod(<.th(_)), <.th()))

  /**
   * @tparam P Persisted data. Data known to be saved.
   * @tparam I Input. A subset of P's fields in a form that matches the editor state.
   * @tparam R Row content prior to being rendered into DOM.
   */
  trait RowRenderer[P, I, R] {
    final type _P = P
    final type _I = I
    final type _R = R
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
                                                                     c           : StateAccessPure[S])
                                                                    (implicit I: DataIdAux[P, K], O: Ordering[RowKey]) {
  /** Row content prior to being rendered into DOM. */
  type RowContent = R
  type RowStream = Stream[(RowKey, VdomElement)]

  private[this] val ST = ReactS.FixCB[S]
  private[this] def run(s: ST.T[Unit]): Callback = c.runState(s)
  private[this] implicit def endofToReactST(f: S => S) = ST modT f

  private[this] val editable = editor.editableByRowStatus(c)

  private[this] val rowLive: savedStore.Row => Live =
    r => live(r.p)

  private[this] def renderRow(a: A, rs: RowStatus): V =
    editor render EditorI(a, "", editable(rs))

  def newButton: VdomElement =
    Button(
      tipe = Button.Type.IconAndText(Icon.Plus, "New"),
      colour = Colour.Green,
      state = Button.State.disabledWhen(newStore.editing(c.state.runNow())))
      .tag(^.onClick --> run(newStore.enableEdit))

  def newCancelButton: VdomElement =
    CancelButton(run(newStore.remove))

  def row(classArg: String, rs: RowStatus, content: RowContent, ctrls: => TagMod): VdomTag = {
    val cls2 = rowStatusRowClass(rs)
    val c = rowStatusCtrls(rs, ctrls)
    <.tr(
      ^.cls := s"$classArg $cls2",
      rr.render(content).toTagMod(<.td(_)),
      <.td(c))
  }

  def newRowO: Option[VdomTag] =
    newStore.get(c.state.runNow()).map(r => {
      val v = renderRow(newRowA(r.i), r.status)
      row("new", r.status, rr.newRow(v), newCancelButton)(^.key := "new")
    })

  def newRow: TagMod =
    newRowO.getOrElse(EmptyVdom)

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

  private def savedLiveRow(rs: RowStatus, p: P, d: Deletion[K]): VdomElement = {
    def del = d.button(p.id, Delete)
    val v = renderRow(savedRowA(p.id), rs)
    row("live", rs, rr.savedRow(v, p), del)(^.key := p.id.value)
  }

  private def savedDeadRow(rs: RowStatus, p: P, d: Deletion[K]): VdomElement = {
    def restore = d.button(p.id, Restore)
    row("dead", rs, rr.deletedRow(p), restore)(^.key := p.id.value)
  }

  def allSortableRows(static: RowStream) =
    MutableArray(static #::: savedRows)
      .sortBy(_._1)
      .map(_._2)
      .array
      .toVdomArray

  def table(header: VdomElement, static: RowStream): VdomElement =
    <.div(
      newButton,
      justTheTable(header, static))

  // I don't care - all this shit will go in the bin soon anyway
  def justTheTable(header: VdomElement, static: RowStream): VdomElement =
    Table.celledCompactUnstackable(
      header,
      <.tbody(newRow, allSortableRows(static)))

  def wrapWithFilterDeadCheckbox(set: SetStateFnPure[FilterDead]): VdomElement => VdomElement = {
    def props: FilterDeadButton.Props =
      StateSnapshot(filterDeadCB.runNow())(set)
    inner => <.div(FilterDeadButton.Component(props), inner)
  }

  // I don't care - all this shit will go in the bin soon anyway
  def wrapWithFilterDeadCheckbox2(setFD: SetStateFnPure[FilterDead], left: VdomElement, table: VdomElement): VdomTag =
    <.div(
      <.div(^.display.flex,
        <.div(^.flex := "1", left),
        <.div(FilterDeadButton.Component(StateSnapshot(filterDeadCB.runNow())(setFD)))),
      table)
}