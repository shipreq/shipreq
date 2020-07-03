package shipreq.webapp.client.project.app.pages.config.reqtypes

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.config.Buttons
import shipreq.webapp.client.project.test.TestConfirmJs

object ReqTypeConfigTestDsl {
  val * = Dsl[TestConfirmJs, ReqTypeConfigObs, Unit]

  val invariants: *.Invariants =
    *.emptyInvariant

  val mnemonicList        = *.focus("Mnemonic list"        ).collection(_.obs.list.rows.map(_.mnemonic))
  val filterDead          = *.focus("FilterDead"           ).value(_.obs.filterDead)
  val isEditorOpen        = *.focus("isEditorOpen"         ).value(_.obs.isEditorOpen)
  val buttonsEnabled      = *.focus("buttonsEnabled"       ).value(_.obs.buttonsEnabled)
  val editorTitle         = *.focus("Editor title"         ).option(_.obs.editorTitle)
  val editorMnemonic      = *.focus("Editor mnemonic"      ).option(_.obs.editor.map(_.mnemonic.value))
  val editorMnemonicError = *.focus("Editor mnemonic error").option(_.obs.editor.flatMap(_.mnemonicError))
  val editorNameError     = *.focus("Editor name error"    ).option(_.obs.editor.flatMap(_.nameError))
  val editorName          = *.focus("Editor name"          ).option(_.obs.editor.map(_.name.value))
  val editorDesc          = *.focus("Editor desc"          ).option(_.obs.editor.map(_.desc.value))
  val pastMnemonics       = *.focus("Past mnemonics"       ).option(_.obs.editor.flatMap(_.pastMnemonics))
  val editorEditables     = *.focus("Editor editables"     ).value(_.obs.editor.fold(0)(_.editables.length))
  val confirms            = *.focus("Confirms"             ).value(_.obs.confirms)

  // ===================================================================================================================

  val clickFilterDead: *.Actions =
    *.action("Click filter dead")(Simulate click _.obs.filterDeadButton)

  def clickUsageLink(mnemonic: String): *.Actions =
    *.action(s"Click $mnemonic usage")(_.obs.list(mnemonic).usage.click())

  def selectReqType(mnemonic: String): *.Actions =
    *.action("Click req type: " + mnemonic)(Simulate click _.obs.list(mnemonic).rowDom)

  def hardDeleteReqType(name: String): *.Actions =
    (selectReqType(name) >> clickHardDeleteButton >> clickCloseButton).group("Hard-delete reqType: " + name)

  def softDeleteReqType(name: String): *.Actions =
    (selectReqType(name) >> clickSoftDeleteButton >> clickCloseButton).group("Soft-delete reqType: " + name)

  def restoreReqType(name: String): *.Actions =
    (selectReqType(name) >> clickRestoreButton >> clickCloseButton).group("Restore reqType: " + name)

  private def clickButton(name: String, f: Buttons[html.Button] => Option[html.Button]): *.Actions =
    *.action("Click button: " + name).attempt(x =>
      f(x.obs.buttonDoms) match {
        case None                  => Some("Button not found")
        case Some(b) if b.disabled => Some("Button is disabled")
        case Some(b)               => Simulate click b; None
      }
    )

  val clickSaveButton       = clickButton("Create/Update"     , _.save)
  val clickCancelButton     = clickButton("Cancel"            , _.cancel)
  val clickCloseButton      = clickButton("Close"             , _.close)
  val clickSoftDeleteButton = clickButton("Delete (soft)"     , _.delete)
  val clickHardDeleteButton = clickButton("Permanently delete", _.hardDel)
  val clickRestoreButton    = clickButton("Restore"           , _.restore)
  val clickAddButton        = clickButton("Add"               , _.add)
  val clickRemoveButton     = clickButton("Remove"            , _.remove)

  def setMnemonic(m: String): *.Actions =
    *.action(s"Set mnemonic to '$m'")(_.obs.editor.get.mnemonic.setValue(m))

  def setName(n: String): *.Actions =
    *.action(s"Set name to '$n'")(_.obs.editor.get.name.setValue(n))

  val clickNew: *.Actions =
    *.action(s"Click new button")(Simulate click _.obs.newButton)

  def setConfirmResponse(b: Boolean): *.Actions =
    *.action("Set next confirm response to " + b)(_.ref.nextResponse = b)
}
