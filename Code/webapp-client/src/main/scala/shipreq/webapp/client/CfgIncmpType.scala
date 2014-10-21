package shipreq.webapp.client

import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import scalaz.std.option._
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import shipreq.base.util.TaggedTypes.taggedStringInstance
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.lib._
import shipreq.webapp.client.util.ui.table._
import shipreq.webapp.client.util.ui.{Editors => E}
import Validators.{customIncmpType => V}
import Routines.CustomIncmpTypeCrud
import DataImplicits._

object CfgIncmpType {

  val tableIO = new TableIO[CustomIncmpTypeAndId, CustomIncmpTypeCrud, CustomIncmpTypeCrud.type]
  import tableIO.{P, D}

  private val prespec = TableSpecBuilder[P](
    FieldSpec[P](_.key.value)(V.key)(E.TextInputEditor),
    FieldSpec[P](_.desc)(V.desc)(E.TextareaEditor))
    .dataId[D]

  private val spec = prespec
    .tableConstraints(
      Some(prespec.uniquenessCheck(_.key).fieldName(FieldNames.refKey)),
      None)
    .saveNotNeededWhenE(p => (p.key, p.desc))
    .asyncSaveP(tableIO.updateIO)

  private val specC = TableSpecC(spec)(tableIO.createIO)

  private val specD = TableSpecD(spec)(_.alive, tableIO.deleteIO)

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

    val tbl = CfgTable[CustomIncmpTypeAndId].b1(spec)(specC, specD, ("", ""), _.key).b2(cells)

    def renderInner(S: ComponentScopeU[tableIO.Props, prespec.S, _]): VDom =
      tbl(S.props.showDeleted, S)(S.props.x)
        .tableness(List(FieldNames.refKey, FieldNames.desc), identity)
  }
}
