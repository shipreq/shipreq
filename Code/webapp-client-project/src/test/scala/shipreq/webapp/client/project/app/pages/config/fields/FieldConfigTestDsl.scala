package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.config.Buttons

object FieldConfigTestDsl {
  val * = Dsl[Unit, FieldConfigObs, Unit]

  val invariants: *.Invariants =
    *.emptyInvariant

  val newChoices          = *.focus("New choices"          ).collection(_.obs.newButton.dropdown.items.map(_.text))
  val fieldList           = *.focus("Field list"           ).collection(_.obs.fieldList.rows.map(_.name))
  val filterDead          = *.focus("FilterDead"           ).value(_.obs.filterDead)
  val isEditorOpen        = *.focus("isEditorOpen"         ).value(_.obs.isEditorOpen)
  val buttonsEnabled      = *.focus("buttonsEnabled"       ).value(_.obs.buttonsEnabled)
  val editorName          = *.focus("Editor name"          ).option(_.obs.editor.flatMap(_.nameValue))
  val editorNameError     = *.focus("Editor name error"    ).option(_.obs.editor.flatMap(_.nameError))
  val editorRules         = *.focus("Editor rules"         ).collection(_.obs.editor.flatMap(_.rules).fold(Vector.empty[RuleRow])(_.rows.map(_.desc)))
  val editorDropdown      = *.focus("Editor dropdown"      ).option(_.obs.editor.flatMap(_.dropdown.flatMap(_.selected)))
  val editorDropdownItems = *.focus("Editor dropdown items").collection(_.obs.editor.iterator.flatMap(_.dropdown).flatMap(_.items).map(_.text).toVector)
  val editorDropdownError = *.focus("Editor dropdown error").value(_.obs.editor.flatMap(_.dropdown.map(_.hasError)).getOrElse(false))
  val messageHeader       = *.focus("Message header"       ).option(_.obs.editor.flatMap(_.message.map(_.header)))
  val editorEditables     = *.focus("Editor editables"     ).value(_.obs.editor.fold(0)(_.editables.length))

  def fieldDetail(name: String) =
    *.focus(s"$name detail").value(_.obs.fieldList(name).detail)

  def ruleResItems(rowIdx: Int): *.FocusColl[Vector, String] =
    *.focus(s"rules[$rowIdx].res items").collection(_
      .obs.editor
      .flatMap(_.rules)
      .flatMap(_.rows.lift(rowIdx))
      .map(_.res.items.map(_.text))
      .getOrElse(Vector.empty))

  def ruleDefaultItems(rowIdx: Int): *.FocusColl[Vector, String] =
    *.focus(s"rules[$rowIdx].default items").collection(_
      .obs.editor
      .flatMap(_.rules)
      .flatMap(_.rows.lift(rowIdx))
      .flatMap(_.default)
      .map(_.items.map(_.text))
      .getOrElse(Vector.empty))

  // ===================================================================================================================

  val clickFilterDead: *.Actions =
    *.action("Click filter dead")(Simulate click _.obs.filterDeadButton)

  def selectField(name: String): *.Actions =
    *.action("Click field: " + name)(Simulate click _.obs.fieldList(name).rowDom)

  def deleteField(name: String): *.Actions =
    (selectField(name) >> clickDeleteButton >> clickCloseButton).group("Delete field: " + name)

  def restoreField(name: String): *.Actions =
    (selectField(name) >> clickRestoreButton >> clickCloseButton).group("Restore field: " + name)

  private def clickButton(name: String, f: Buttons[html.Button] => Option[html.Button]): *.Actions =
    *.action("Click button: " + name).attempt(x =>
      f(x.obs.buttonDoms) match {
        case None                  => Some("Button not found")
        case Some(b) if b.disabled => Some("Button is disabled")
        case Some(b)               => Simulate click b; None
      }
    )

  val clickSaveButton    = clickButton("Create/Update", _.save)
  val clickCancelButton  = clickButton("Cancel"       , _.cancel)
  val clickCloseButton   = clickButton("Close"        , _.close)
  val clickDeleteButton  = clickButton("Delete"       , _.delete)
  val clickRestoreButton = clickButton("Restore"      , _.restore)
  val clickAddButton     = clickButton("Add"          , _.add)
  val clickRemoveButton  = clickButton("Remove"       , _.remove)

  def setEditorName(name: String): *.Actions =
    *.action(s"Set editor name to: $name")(SimEvent.Change(name) simulate _.obs.editor.get.nameDom.get)

  val addEditorRule: *.Actions =
    *.action("Add editor rule")(Simulate click _.obs.editor.get.rules.get.rows.last.addButton.get)

  def delEditorRule(rowIdx: Int): *.Actions =
    *.action(s"Delete editor rule $rowIdx")(Simulate click _.obs.editor.get.rules.get.rows(rowIdx).delButton.get)

  def setRuleReqTypes(rowIdx: Int, txt: String): *.Actions =
    *.action(s"Set rules[$rowIdx].reqTypes to: $txt")(SimEvent.Change(txt) simulate _.obs.editor.get.rules.get.rows(rowIdx).reqTypesDom.get)

  def setRuleReqRes(rowIdx: Int, res: String): *.Actions =
    *.action(s"Set rules[$rowIdx].res to: $res")(_.obs.editor.get.rules.get.rows(rowIdx).res.select(res))

  def setRuleDefault(rowIdx: Int, default: String): *.Actions =
    *.action(s"Set rules[$rowIdx].default to: $default")(_.obs.editor.get.rules.get.rows(rowIdx).default.get.select(default))

  def selectNew(name: String): *.Actions =
    *.action(s"New button: select $name")(_.obs.newButton.dropdown.select(name))

  val clickNew: *.Actions =
    *.action(s"Click new button")(_.obs.newButton.click())

  def clickNew(name: String): *.Actions =
    selectNew(name) >> clickNew

  def setEditorDropdown(name: String): *.Actions =
    *.action(s"Set editor dropdown to: $name")(_.obs.editor.get.dropdown.get.select(name))

}
