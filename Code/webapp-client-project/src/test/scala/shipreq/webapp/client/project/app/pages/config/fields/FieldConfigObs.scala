package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import org.scalajs.dom.html
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.ui.semantic.Input
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.pages.config.Buttons
import shipreq.webapp.client.project.test.CommonObs

object FieldConfigObs {

  lazy val selNewButton         = Style.widgets.dropdownButtonOuter       .selector
  lazy val selRightOn           = Style.widgets.splitScreenCrud.rightOn   .selector
  lazy val selEmptyRight        = Style.widgets.splitScreenCrud.emptyRight.selector
  lazy val selEditorButtons     = Style.tagConfig.editorButtons           .selector
  lazy val selRulesDeadReqTypes = Style.fieldConfig.rulesDeadReqTypesInner.selector

  final class FieldList($: DomZipperJs) {
    val rows: Vector[FieldListRow] =
      $("tbody").children1n("tr").map(new FieldListRow(_))

    def apply(name: String): FieldListRow =
      rows.find(_.name ==* name).getOrElse(throw new RuntimeException("Field not found: " + name))
  }

  final class FieldListRow($: DomZipperJs) {
    val rowDom = $.domAsHtml
    val name   = $.child("td", 2 of 5).innerText
    val detail = $.child("td", 4 of 5).innerText
  }

  final class Editor($: DomZipperJs) {
    private val soleField = $.collect01(".ui.form .field")

    val message = $.collect01(".ui.message").map(new CommonObs.Message(_))

    val nameDom   = soleField.zippers.flatMap(_.collect01("input").domsAs[html.Input])
    val nameValue = nameDom.map(_.value)
    val nameError = soleField.zippers.filter(_ => nameDom.isDefined).flatMap(_.children01("span").innerTexts)

    val dropdown = soleField.zippers.filter(_.exists(".menu")).map(new CommonObs.Dropdown(_))

    val rules = $.collect01("table.ui.single.line").map(new Rules(_))
  }

  final class Rules($: DomZipperJs) {
    val rows = $.child("tbody").children1n.map(new RuleRowObs(_))
  }

  final class RuleRowObs($: DomZipperJs) {
    private val reqTypes = $.child("td", 1 of 3)
    private val rule     = $.child("td", 2 of 3)
    private val button   = $.child("td", 3 of 3)

    val reqTypesDom   = reqTypes.collect01("input").domsAs[html.Input]
    val reqTypesError = reqTypes.collect01(s"[${Input.errorAttr.attrName}]").innerTexts
    val deadReqTypes  = reqTypes.collect01(selRulesDeadReqTypes).innerTexts
    val res           = new CommonObs.Dropdown(rule(".ui.dropdown.selection:first-child"))
    val default       = rule.collect01(".ui.dropdown.selection:not(:first-child)").map(new CommonObs.Dropdown(_))
    val dead          = button.collect01("button").isEmpty

    val reqTypesDesc: String =
      reqTypesDom match {
        case Some(r) if !dead => r.value
        case _                => reqTypes.innerText.replace("Dead req types:", "").trim
      }

    val desc = RuleRow(
      reqTypes      = reqTypesDesc,
      rule          = res.selected.getOrElse(""),
      default       = default.map(_.selected.getOrElse("")),
      defaultError  = default.fold(false)(_.hasError),
      dead          = dead,
      reqTypesError = reqTypesError,
    )

    val addButton = button.collect01("button.green").domsAsHtml
    val delButton = button.collect01("button.negative").domsAsHtml
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

  val editor: Option[Editor] =
    Option.when(isEditorOpen)(new Editor(right.child("div").child("div", 1 of 2)))

  val newButton =
    new CommonObs.DropdownButton(left(selNewButton))
}
