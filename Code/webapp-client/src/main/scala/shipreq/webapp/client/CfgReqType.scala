package shipreq.webapp.client

import scalaz.std.anyVal.booleanInstance
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.experiment.OnUnmount

import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.lib.{CfgTableCells, CfgTable, TableIO}
import shipreq.webapp.client.util.ui.table._
import shipreq.webapp.client.util.ui.{Editors => E, Util}
import Validators.{reqType => V}
import ReqType.Mnemonic
import Routines.CustomReqTypeCrud
import DataImplicits._

object CfgReqType {

  val tableIO = new TableIO[CustomReqTypeAndId, CustomReqTypeCrud, CustomReqTypeCrud.type]
  import tableIO.{P, D, Arb}

  private val prespec = TableSpecBuilder[P](
    FieldSpec[P](_.mnemonic.value)(V.mnemonic)(E.TextInputEditor),
    FieldSpec[P](_.name)(V.name)(E.TextInputEditor),
    FieldSpec[P].noValidation(_.imp, ImplicationRequired)(E.CheckboxEditor))
    .dataId[D]

  private def mnemonicUniqueness =
    TableConstraint.uniquenessE[prespec.S, prespec.R, Mnemonic](
      (s, r) => {
        val custom: Stream[ReqType] =
          s._1.toStream
            .filterNot(dpi => r.fold(false)(_ == dpi._1)) // exclude own row
            .map(_._2.p)
        val static: Stream[ReqType] = ReqType.static.toStream
        (static #::: custom).flatMap(p => p.mnemonic #:: p.oldMnemonics.toStream)
      }).fieldName("Mnemonic")

  private val spec = prespec
    .tableConstraints(
      Some(mnemonicUniqueness),
      Some(prespec.uniquenessCheck(_.name).fieldName("Name")),
      None)
    .saveNotNeededWhenE(p => (p.mnemonic, p.name, p.imp))
    .asyncSaveP(_.id, tableIO.saveIO)

  private val deletion =
    new AsyncDeletion(spec)(_.alive, tableIO.deleteIO)

  private val newRowS =
    spec.unsavedInitS(("","",false))

  // ===================================================================================================================
  // Component

  case class Props(x: Arb, showDeleted: Boolean)

  private final class Backend extends OnUnmount

  val Component = ReactComponentB[Props]("CfgReqTypes")
    .getInitialState(p => p.showDeleted)
    .render(Render.renderOuter _)
    .build

  private val InnerComponent = ReactComponentB[Props]("CfgReqTypesⁱ")
    .getInitialState(p => spec.initialState(p.x._2.project.customReqTypes.data, _.id))
    .backend(_ => new Backend)
    .render(Render.renderInner _)
    .configure(tableIO.recvExtUpdates(spec, Partition.CustomReqTypes, _.x))
    .build

  // ===================================================================================================================
  private object Render {
    import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
    import Util.checkbox

    val cells = new CfgTableCells[P, spec.VV, (Modifier, Set[ReqType.Mnemonic], Modifier, Modifier)] {
      override def mklist = {
        case (mnemonic, oldMnemonics, name, impReq) => {
          val mn: Modifier =
            if (oldMnemonics.isEmpty)
              mnemonic
            else
              Seq(mnemonic, div(cls := "oldMnemonics", oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
          List(mn, name, impReq)
        }
      }
      override def newRow = {
        case (mnemonic, name, impReq) => (mnemonic, Set.empty, name, impReq)
      }
      override def savedRow = {
        case ((mnemonic, name, impReq), p) => (mnemonic, p.oldMnemonics, name, impReq)
      }
      override def deletedRow = {
        case p => (raw(p.mnemonic), p.oldMnemonics, raw(p.name), checkbox(ImplicationRequired from p.imp)(disabled := true))
      }
    }

    val tbl = CfgTable[CustomReqTypeAndId].b1(spec)(deletion, ("", "", false), _.mnemonic).b2(cells)
    import tbl.RowStream

    def renderOuter(S: ComponentScopeU[Props, Boolean, Unit]): VDom = {
      val s = S.state
      div(
        label(
          checkbox(s)(onchange --> S.modState(b => !b)),
          raw(if (s) "Showing deleted" else "Not showing deleted")),
        InnerComponent(S.props.copy(showDeleted = s)))
    }

    def renderInner(S: ComponentScopeU[Props, prespec.S, Backend]): VDom =
      tbl(S.props.showDeleted, S)(S.props.x)
        .tableness(List("Mnemonic", "Name", "Implication Required"), staticRows #::: _)

    val staticRows: RowStream = {
      def rr(r: ReqType.Static) = {
        val imp = checkbox(ImplicationRequired from r.imp)(disabled := true)
        tbl.row("static", RowStatus.Sync, (raw(r.mnemonic), r.oldMnemonics, raw(r.name), imp), EmptyTag)(keyAttr := r.mnemonic.value)
      }
      ReqType.static.map(r => r.mnemonic -> rr(r)).toStream
    }
  }
}