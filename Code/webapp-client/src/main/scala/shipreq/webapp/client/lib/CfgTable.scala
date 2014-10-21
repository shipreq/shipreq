package shipreq.webapp.client.lib

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.DeletionAction._
import shipreq.webapp.client.util.ui.table._
import DataImplicits._

trait CfgTableCells[P, VV, Cells] {
  def mklist: Cells => List[Modifier]
  def newRow: VV => Cells
  def savedRow: (VV, P) => Cells
  def deletedRow: P => Cells
}

object CfgTable {
  @inline final def apply[DI <: DataAndId : IdAccessor] = new CfgTable[DI]
}

final class CfgTable[DI <: DataAndId : IdAccessor] {
  type P = DI#Data
  type D = DI#Id

  // -------------------------------------------------------------------------------------------------------------------
  @inline def b1[Arb, S, U, II, VV, RowKey: Ordering](specU: TableSpecU[Arb, S, D, U, P, II, VV])
                                                     (specC: TableSpecC[Arb, S, D, U, P, II, VV],
                                                      specD: TableSpecD[Arb, S, P, D],
                                                      emptyII: II,
                                                      rowkey: P => RowKey) =
    new B1(specU, specC, specD, emptyII, rowkey)

  final class B1[Arb, S, U, II, VV, RowKey: Ordering](specU: TableSpecU[Arb, S, D, U, P, II, VV], specC: TableSpecC[Arb, S, D, U, P, II, VV], specD: TableSpecD[Arb, S, P, D], emptyII: II, rowkey: P => RowKey) {

    val newRowS =
      specC.unsavedInitS(emptyII)

    // -----------------------------------------------------------------------------------------------------------------
    @inline def b2[Cells](cells: CfgTableCells[P, VV, Cells]) = new B2(cells)

    final class B2[Cells](cells: CfgTableCells[P, VV, Cells]) {
      type RowStream = Stream[(RowKey, Tag)]

      def row(classArg: String, rs: RowStatus, vv: Cells, ctrls: => Modifier): Tag = {
        val (cls2, c: Modifier) = rs match {
          case RowStatus.Sync          => ("sync", ctrls)
          case RowStatus.Locked        => ("locked", img(cls := "spinner", src := "/assets/loading-spin.svg"))
          case RowStatus.Failed(retry) => ("failed", button("Retry", onclick ~~> retry))
        }
        tr(cls := s"$classArg $cls2", cells.mklist(vv).map(td(_)), td(c))
      }

      // ---------------------------------------------------------------------------------------------------------------
      @inline def apply(showDeleted: Boolean, S: ComponentStateFocus[S])(implicit x: Arb) =
        new B3(showDeleted, S)

      final class B3(showDeleted: Boolean, S: ComponentStateFocus[S])(implicit x: Arb) {

        def newButton =
          button(onclick ~~> S.runState(newRowS), disabled := specC.unsavedRowExists(S), "New")

        def newRow =
          specC.unsavedRow((F, rs, vv) => {
            def c = button(onclick ~~> F.runState(specC.unsavedRemoveS), "Cancel")
            row("new", rs, cells.newRow(vv), c)(keyAttr := "new")
          })(x)(S)

        private def savedRow =
          specU.savedRowP((F, id, rs, p, vv) => {
            def c = specD.buttons(F, id, HardDel, SoftDel)
            row("live", rs, cells.savedRow(vv, p), c)(keyAttr := id.value)
          })(x)(S)

        def savedRows: RowStream =
          specD.savedGetP(S, Alive).map(p => rowkey(p) -> savedRow(p.id))

        private def deletedRow(rs: RowStatus, p: P) = {
          def c = specD.button(S, p.id, Restore)
          row("dead", rs, cells.deletedRow(p), c)(keyAttr := p.id.value)
        }

        def deletedRows: RowStream =
          if (showDeleted)
            specD.savedGet(S, Dead).map(r => rowkey(r.p) -> deletedRow(r.status, r.p))
          else
            Stream.empty

        def nonNewRows(rows: RowStream => RowStream) =
          rows(deletedRows #::: savedRows).sortBy(_._1).map(_._2).toJsArray

        def tableness(headers: List[String], rows: RowStream => RowStream) =
          div(
            newButton,
            table(
              thead(tr(headers.map(th(_)), th("Ctrls"))),
              tbody(newRow, nonNewRows(rows))))
      }
    }
  }
}