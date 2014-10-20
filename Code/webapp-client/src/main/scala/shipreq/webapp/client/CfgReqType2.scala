package shipreq.webapp.client

import shipreq.webapp.client.protocol.FailureIO

import scalaz.effect.IO
import scalaz.std.anyVal.booleanInstance
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import japgolly.scalajs.react.ReactComponentB

import shipreq.base.util.TaggedTypes.taggedStringInstance
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.lib.{CfgTableCells, CfgTable, TableIO, RemoteDeltaListener}
import shipreq.webapp.client.util.OnUnmountBackend
import shipreq.webapp.client.util.ui.table._
import shipreq.webapp.client.util.ui.{Editors => E, Util}
import Validators.{reqType => V}
import ReqType.Mnemonic
import Routines.CustomReqTypeImpUpd
import DataImplicits._


object CfgReqType2 {

//  val tableIO = new TableIO[CustomReqTypeAndId, CustomReqTypeCrud, CustomReqTypeCrud.type]
  val tableIO = new RemoteDeltaListener[CustomReqTypeAndId, CustomReqTypeImpUpd.type]
  import tableIO.{P, D, Arb}
//  type P = CustomReqType
//  type D = CustomReqType.Id
//  type Arb = (CustomReqTypeImpUpd.Remote, ClientData)

  private val prespec = TableSpecBuilder[P](
    FieldSpec[P].noValidation(_.imp, ImplicationRequired)(E.CheckboxEditor))
    .dataId[D]

  private val spec = prespec
    .tableConstraints(None)
    .saveNotNeededWhenE(_.imp)
    .asyncSaveP(_.id, saveIO)

  import prespec.U

  def saveIO(arb: Arb, op: Option[P], u: U, s: SuccessIO, f: FailureIO): IO[Unit] = ???

/*
  private val deletion = new AsyncDeletion(spec)(_.alive, tableIO.deleteIO)

  private val innerComponent = tableIO.innerComponent(spec, Partition.CustomIncmpTypes, Render.renderInner)

  val Component = tableIO.outerComponent("CfgIncmpTypes", innerComponent)

  // ===================================================================================================================
  private object Render {
    import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._

    val cells = new CfgTableCells[P, spec.VV, spec.VV] {
      override def mklist = { case (key, desc) => List(key, desc) }
      override def newRow = identity
      override def savedRow = (v,p) => v
      override def deletedRow = p => (raw(p.key.value), raw(TextMod.nonBlank from p.desc))
    }

    val tbl = CfgTable[CustomIncmpTypeAndId].b1(spec)(deletion, ("", ""), _.key).b2(cells)

    def renderInner(S: ComponentScopeU[tableIO.Props, prespec.S, _]): VDom =
      tbl(S.props.showDeleted, S)(S.props.x)
        .tableness(List(FieldNames.refKey, FieldNames.desc), identity)
  }
  */
}