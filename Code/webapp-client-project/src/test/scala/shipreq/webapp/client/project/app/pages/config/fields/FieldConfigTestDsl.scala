package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.config.Buttons

object FieldConfigTestDsl {
  val * = Dsl[Unit, FieldConfigObs, Unit]

  val invariants: *.Invariants =
    *.emptyInvariant

  val fieldList       = *.focus("Field list"       ).collection(_.obs.fieldList.rows.map(_.name))
  val filterDead      = *.focus("FilterDead"       ).value(_.obs.filterDead)
  val isEditorOpen    = *.focus("isEditorOpen"     ).value(_.obs.isEditorOpen)
  val buttonsEnabled  = *.focus("buttonsEnabled"   ).value(_.obs.buttonsEnabled)
  val editorName      = *.focus("Editor name"      ).option(_.obs.editor.flatMap(_.nameValue))
  val editorNameError = *.focus("Editor name error").option(_.obs.editor.flatMap(_.nameError))
  val editorRules     = *.focus("Editor rules"     ).collection(_.obs.editor.fold(Vector.empty[RuleRow])(_.rules.rows.map(_.desc)))

  val clickFilterDead: *.Actions =
    *.action("Click filter dead")(Simulate click _.obs.filterDeadButton)

  def selectField(name: String): *.Actions =
    // TODO Pending https://github.com/japgolly/scalajs-react/issues/674
    *.action("Click field: " + name)(x => Simulate.click(x.obs.fieldList(name).rowDom, scalajs.js.Dynamic.literal(defaultPrevented = false)))

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

  def setEditorName(name: String): *.Actions =
    *.action(s"Set editor name to: $name")(SimEvent.Change(name) simulate _.obs.editor.get.nameDom.get)

  val addEditorRule: *.Actions =
    *.action("Add editor rule")(Simulate click _.obs.editor.get.rules.rows.last.addButton.get)

  def delEditorRule(rowIdx: Int): *.Actions =
    *.action(s"Delete editor rule $rowIdx")(Simulate click _.obs.editor.get.rules.rows(rowIdx).delButton.get)

  def setRuleReqTypes(rowIdx: Int, txt: String): *.Actions =
    *.action(s"Set rules[$rowIdx].reqTypes to: $txt")(SimEvent.Change(txt) simulate _.obs.editor.get.rules.rows(rowIdx).reqTypesDom.get)

  def setRuleReqRes(rowIdx: Int, res: String): *.Actions =
    *.action(s"Set rules[$rowIdx].res to: $res")(_.obs.editor.get.rules.rows(rowIdx).res.select(res))
}
