package shipreq.webapp.client.ui

import scalaz.std.anyVal.booleanInstance
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import shipreq.base.util.TaggedTypes.taggedStringInstance
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.Validators.{reqType => V}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines.CustomReqTypeCrud
import shipreq.webapp.client.lib._
import shipreq.webapp.client.util.ui.table._
import shipreq.webapp.client.util.ui.{Editors => E}

object CfgReqType {

  val tableIO = new TableIO[CustomReqTypeAndId, CustomReqTypeCrud, CustomReqTypeCrud.type]
  import tableIO.{D, P}

  private val prespec = TableSpecBuilder[P](
    FieldSpec[P](_.mnemonic.value)(V.mnemonic)(E.TextInputEditor),
    FieldSpec[P](_.name)(V.name)(E.TextInputEditor),
    FieldSpec[P].noValidation(_.imp, ImplicationRequired)(E.CheckboxEditor))
    .dataId[D]

  private val spec = prespec
    .tableConstraints(
      Some(mnemonicUniqueness),
      Some(prespec.uniquenessCheck(_.name).fieldName("Name")),
      None)
    .saveNotNeededWhenE(p => (p.mnemonic, p.name, p.imp))
    .asyncSaveP(tableIO.updateIO)

  private val specC = TableSpecC(spec)(tableIO.createIO)

  private val specD = TableSpecD(spec)(_.alive, tableIO.deleteIO)

  private val innerComponent = tableIO.innerComponent(spec, Partition.CustomReqTypes, Render.renderInner)

  val Component = tableIO.outerComponent("CfgReqTypes", innerComponent)

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

  // ===================================================================================================================
  private object Render {
    import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._
    import shipreq.webapp.client.util.ui.Util.checkbox

    val cells = new CfgTableCells[P, spec.VV, (Modifier, Set[ReqType.Mnemonic], Modifier, Modifier)] {
      override def mklist = {
        case (mnemonic, oldMnemonics, name, impReq) =>
          val mn: Modifier =
            if (oldMnemonics.isEmpty)
              mnemonic
            else
              Seq(mnemonic, div(cls := "oldMnemonics", oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
          List(mn, name, impReq)
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

    val tbl = CfgTable[CustomReqTypeAndId].b1(spec)(specC, specD, ("", "", false), _.mnemonic).b2(cells)

    def renderInner(S: ComponentScopeU[tableIO.Props, prespec.S, _]): VDom =
      tbl(S.props.showDeleted, S)(S.props.x)
        .tableness(List("Mnemonic", "Name", "Implication Required"), staticRows #::: _)

    val staticRows: tbl.RowStream = {
      def rr(r: ReqType.Static) = {
        val imp = checkbox(ImplicationRequired from r.imp)(disabled := true)
        tbl.row("static", RowStatus.Sync, (raw(r.mnemonic), r.oldMnemonics, raw(r.name), imp), EmptyTag)(keyAttr := r.mnemonic.value)
      }
      ReqType.static.map(r => r.mnemonic -> rr(r)).toStream
    }
  }
}