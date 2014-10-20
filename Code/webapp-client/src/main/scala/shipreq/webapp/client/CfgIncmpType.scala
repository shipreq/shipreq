package shipreq.webapp.client

import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import scalaz.std.option._
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import japgolly.scalajs.react.ReactComponentB
import shipreq.base.util.TaggedTypes.taggedStringInstance
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.lib._
import shipreq.webapp.client.util.ui.table._
import shipreq.webapp.client.util.ui.{Editors => E, Util}
import Validators.{customIncmpType => V}
import Routines.CustomIncmpTypeCrud
import DataImplicits._

object CfgIncmpType {

  val tableIO = new TableIO[CustomIncmpTypeAndId, CustomIncmpTypeCrud, CustomIncmpTypeCrud.type]
  import tableIO.{P, D, Arb}

  private val prespec = TableSpecBuilder[P](
    FieldSpec[P](_.key.value)(V.key)(E.TextInputEditor),
    FieldSpec[P](_.desc)(V.desc)(E.TextareaEditor))
    .dataId[D]

  private val spec = prespec
    .tableConstraints(
      Some(prespec.uniquenessCheck(_.key).fieldName(FieldNames.refKey)),
      None)
    .saveNotNeededWhenE(p => (p.key, p.desc))
    .asyncSaveP(_.id, tableIO.saveIO)

  private val deletion = new AsyncDeletion(spec)(_.alive, tableIO.deleteIO)

  // ===================================================================================================================
  // Component

  case class Props(x: Arb, showDeleted: Boolean)

  val Component = ReactComponentB[Props]("CfgIncmpTypes")
    .getInitialState(p => p.showDeleted)
    .render(Render.renderOuter _)
    .build

  private val innerComponent = tableIO.innerComponent(spec, Partition.CustomIncmpTypes, Render.renderInner)

  // ===================================================================================================================
  private object Render {
    import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
    import Util.checkbox

    val cells = new CfgTableCells[P, spec.VV, spec.VV] {
      override def mklist = { case (key, desc) => List(key, desc) }
      override def newRow = identity
      override def savedRow = (v,p) => v
      override def deletedRow = p => (raw(p.key.value), raw(TextMod.nonBlank from p.desc))
    }

    val tbl = CfgTable[CustomIncmpTypeAndId].b1(spec)(deletion, ("", ""), _.key).b2(cells)

    def renderOuter(S: ComponentScopeU[Props, Boolean, Unit]): VDom = {
      val s = S.state
      div(
        label(
          checkbox(s)(onchange --> S.modState(b => !b)),
          raw(if (s) "Showing deleted" else "Not showing deleted")),
        innerComponent(TableIoProps(S.props.x, s)))
    }

    def renderInner(S: ComponentScopeU[tableIO.Props, prespec.S, _]): VDom =
      tbl(S.props.showDeleted, S)(S.props.x)
        .tableness(List(FieldNames.refKey, FieldNames.desc), identity)
  }
}
