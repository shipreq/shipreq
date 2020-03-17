package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.config.Buttons

object FieldConfigTestDsl {
  val * = Dsl[Unit, FieldConfigObs, Unit]

  val invariants: *.Invariants =
    *.emptyInvariant

  val fieldList      = *.focus("Field list"    ).collection(_.obs.fieldList.rows.map(_.name))
  val filterDead     = *.focus("FilterDead"    ).value(_.obs.filterDead)
  val isEditorOpen   = *.focus("isEditorOpen"  ).value(_.obs.isEditorOpen)
  val buttonsEnabled = *.focus("buttonsEnabled").value(_.obs.buttonsEnabled)

  val clickFilterDead: *.Actions =
    *.action("Click filter dead")(Simulate click _.obs.filterDeadButton)

//  def selectField(name: String): *.Actions =
////    *.action("Click tag: " + name)(Simulate click _.obs.tagTreeLI(name).rowDom)
//    // TODO Pending https://github.com/japgolly/scalajs-react/issues/674
//    *.action("Click tag: " + name)(x => Simulate.click(x.obs.tagTreeLI(name).rowDom, scalajs.js.Dynamic.literal(defaultPrevented = false)
//    ))
//
//  def deleteField(name: String): *.Actions =
//    (selectField(name) >> clickDeleteButton >> clickCloseButton).group("Delete tag: " + name)
//
//  def restoreField(name: String): *.Actions =
//    (selectField(name) >> clickRestoreButton >> clickCloseButton).group("Restore tag: " + name)

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

}
