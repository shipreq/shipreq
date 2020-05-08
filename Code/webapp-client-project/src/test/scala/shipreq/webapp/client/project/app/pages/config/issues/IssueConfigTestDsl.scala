package shipreq.webapp.client.project.app.pages.config.issues

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.config.Buttons

object IssueConfigTestDsl {
  val * = Dsl[Unit, IssueConfigObs, Unit]

  val invariants: *.Invariants =
    *.emptyInvariant

  val keyList         = *.focus("Key list"        ).collection(_.obs.list.rows.map(_.key))
  val filterDead      = *.focus("FilterDead"      ).value(_.obs.filterDead)
  val isEditorOpen    = *.focus("isEditorOpen"    ).value(_.obs.isEditorOpen)
  val buttonsEnabled  = *.focus("buttonsEnabled"  ).value(_.obs.buttonsEnabled)
  val otherSources    = *.focus("Other sources"   ).option(_.obs.otherSources.map(_.consolidation))
  val editorTitle     = *.focus("Editor title"    ).option(_.obs.editor.map(_.editorTitle))
  val editorKey       = *.focus("Editor key"      ).option(_.obs.editor.map(_.key.value))
  val editorKeyError  = *.focus("Editor key error").option(_.obs.editor.flatMap(_.keyError))
  val editorDesc      = *.focus("Editor desc"     ).option(_.obs.editor.map(_.desc.value))
  val editorEditables = *.focus("Editor editables").value(_.obs.editor.fold(0)(_.editables.length))

  // ===================================================================================================================

  val clickFilterDead: *.Actions =
    *.action("Click filter dead")(Simulate click _.obs.filterDeadButton)

  def clickUsageLink(key: String): *.Actions =
    *.action(s"Click $key usage")(_.obs.list(key).usage.click())

  def selectIssue(key: String): *.Actions =
    *.action("Click issue: " + key)(Simulate click _.obs.list(key).rowDom)

  def deleteIssue(name: String): *.Actions =
    (selectIssue(name) >> clickDeleteButton >> clickCloseButton).group("Delete issue: " + name)

  def restoreIssue(name: String): *.Actions =
    (selectIssue(name) >> clickRestoreButton >> clickCloseButton).group("Restore issue: " + name)

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

  def setKey(m: String): *.Actions =
    *.action(s"Set key to '$m'")(_.obs.editor.get.key.setValue(m))

  def setDesc(n: String): *.Actions =
    *.action(s"Set desc to '$n'")(_.obs.editor.get.desc.setValue(n))

  val clickNew: *.Actions =
    *.action(s"Click new button")(Simulate click _.obs.newButton)
}
