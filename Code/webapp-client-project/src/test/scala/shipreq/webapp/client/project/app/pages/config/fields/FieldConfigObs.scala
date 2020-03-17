package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.scalajs.dom.html
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.pages.config.Buttons

object FieldConfigObs {

  lazy val selRightOn       = Style.widgets.splitScreenCrud.rightOn        .selector
  lazy val selEmptyRight    = Style.widgets.splitScreenCrud.emptyRight     .selector
  lazy val selEditorButtons = Style.tagConfig.editorButtons                .selector

  final class FieldList($: DomZipperJs) {
    val rows: Vector[FieldListRow] =
      $("tbody").children1n("tr").map(new FieldListRow(_))
  }

  final class FieldListRow($: DomZipperJs) {
    val name = $.child("td", 2 of 5).innerText
  }
}

// =====================================================================================================================

final class FieldConfigObs($: DomZipperJs) {
  import FieldConfigObs._

  val left  = $.child("section", 1 of 2)
  val right = $.child("section", 2 of 2).child(selRightOn)

  val filterDeadButton = left.child("div").child("div", 2 of 2)("button").dom
  val filterDead       = ShowDead.when(filterDeadButton.classList.contains("red"))

  val fieldList = new FieldList(left)

  val isEditorOpen: Boolean =
    !right.exists(selEmptyRight)

  val buttonDoms: Buttons[html.Button] =
    if (isEditorOpen)
      Buttons.obs(right(selEditorButtons))
    else
      Buttons.none

  val buttonsEnabled: Buttons[Enabled] =
    buttonDoms.map(Disabled when _.disabled)

}
