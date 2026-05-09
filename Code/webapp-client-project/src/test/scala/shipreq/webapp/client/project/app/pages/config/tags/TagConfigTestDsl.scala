package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.config.Buttons
import shipreq.webapp.member.test.CommonObs

object TagConfigTestDsl {
  val * = Dsl[Unit, TagConfigObs, Unit]

  val invariants: *.Invariants =
    *.emptyInvariant

  val tagTreeText          = *.focus("Tag tree"            ).value(_.obs.tagTree.text)
  val filterDead           = *.focus("FilterDead"          ).value(_.obs.filterDead)
  val isEditorOpen         = *.focus("isEditorOpen"        ).value(_.obs.isEditorOpen)
  val buttonsEnabled       = *.focus("buttonsEnabled"      ).value(_.obs.buttonsEnabled)
  val parentsText          = *.focus("parentsText"         ).option(_.obs.editorParents.map(_.text))
  val childrenText         = *.focus("childrenText"        ).option(_.obs.editorChildren.map(_.text))
  val reqTypesText         = *.focus("reqTypesText"        ).option(_.obs.applicableReqTypes.map(_.inputValue))
  val reqTypesDead         = *.focus("reqTypesDead"        ).option(_.obs.applicableReqTypes.flatMap(_.dead))
  val reqTypesError        = *.focus("reqTypesError"       ).option(_.obs.applicableReqTypes.flatMap(_.error))
  val reqTypeApplicability = *.focus("reqTypeApplicability").option(_.obs.applicableReqTypes.map(_.selected))
  val nameEditorValue      = *.focus("Name editor value"   ).option(_.obs.nameEditor.map(_.value))

  val newButton = new CommonObs.DropdownButton.TestDsl(*, "New")(_.newButton)

  val clickFilterDead: *.Actions =
    *.action("Click filter dead")(Simulate click _.obs.filterDeadButton)

  def selectTag(name: String): *.Actions =
    *.action("Click tag: " + name)(Simulate click _.obs.tagTreeLI(name).rowDom)

  def deleteTag(name: String): *.Actions =
    (selectTag(name) >> clickDeleteButton >> clickCloseButton).group("Delete tag: " + name)

  def restoreTag(name: String): *.Actions =
    (selectTag(name) >> clickRestoreButton >> clickCloseButton).group("Restore tag: " + name)

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

  private def addRel(rel: String, getEditor: TagConfigObs => Option[TagConfigObs.RelTree])(name: String): *.Actions =
    *.action(s"Add $rel: $name").attempt(x =>
      getEditor(x.obs).map(_.addItem(name)) match {
        case Some(Some(i)) => Simulate click i.dom; None
        case Some(None)    => Some("Item not found in add button: " + name)
        case None          => Some("Editor not open")
      }
    )

  def addChild(name: String): *.Actions =
    addRel("child", _.editorChildren)(name)

  def addParent(name: String): *.Actions =
    addRel("parent", _.editorParents)(name)

  def setReqTypeApplicability(txt: String): *.Actions =
    *.action("setReqTypeApplicability: " + txt)(Simulate click _.obs.applicableReqTypes.get.items.find(_.textContent.trim == txt).get)

  def setApplicableReqTypesText(txt: String): *.Actions =
    *.action("setApplicableReqTypesText: " + txt)(SimEvent.Change(txt) simulate _.obs.applicableReqTypes.get.inputDom)

  def setNameEditorValue(v: String): *.Actions =
    *.action(s"Set name editor to ${v.quote}")(_.obs.nameEditor.getOrThrow("Name editor unavailable").setValue(v))
}
