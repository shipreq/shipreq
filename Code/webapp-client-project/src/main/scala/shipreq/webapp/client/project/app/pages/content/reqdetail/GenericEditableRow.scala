package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets._

private[reqdetail] object GenericEditableRow {

  final case class Props(row       : Row,
                         name      : String,
                         headerLive: Live,
                         dataLive  : Live,
                         field     : FieldKey.ForSomeReq#AndArgs,
                         editor    : EditorFeature.ReadWrite.ForAnyEditor,
                         view      : Reusable[ViewReq[VdomTag]],
                        ) {
    @inline def render: VdomElement = Component.withKey(row.key)(this)
  }

  implicit val reusabilityProps: Reusability[Props] = {
    @nowarn("cat=unused")
    implicit def x: Reusability[FieldKey.ForSomeReq#AndArgs] = FieldKey.AndArgs.reusability.narrow

    Reusability.derive
  }

  private def render(p: Props): VdomNode =
    Shared.renderRow(
      row        = p.row,
      name       = p.name,
      headerLive = p.headerLive,
      dataLive   = p.dataLive,
    )(renderRowData(_, p))

  private def renderRowData(cell: Shared.DataCell, p: Props): VdomNode = {
    // I really couldn't be fucked adding existential types to Props and doing all the manual boilerplate around it
    // TODO Fix in Scala 3
    val editor = p.editor.asInstanceOf[EditorFeature.ReadWrite.For[p.field.key.type]]

    cell.editorNavParent(editor, p.field.args, p.view.editable(p.field.key).getOrElse(EmptyVdom))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
