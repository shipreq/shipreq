package shipreq.webapp.client.project.app.pages.admin.access

import shipreq.webapp.base.data.ProjectRole
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.test._

object AccessPageTestDsl {

  final case class Ref(global   : TestGlobal,
                       confirmJs: TestConfirmJs)

  val * = Dsl[Ref, AccessPageObs, Unit]

  val global = new TestGlobal.TestDslWithObs(*)(_.global, _.global)

  val confirmJs = new TestConfirmJs.TestDsl(*)(_.confirmJs, _.confirmJs)

  val invariants: *.Invariants =
    *.emptyInvariant

  // ===================================================================================================================

  val addButtonStatus           = *.focus("Add button status").value(_.obs.addButtonStatus)
  val newUserInput              = *.focus("New user input").value(_.obs.newUserInput.value)
  val newUserInputEnabled       = *.focus("New user input enabled").value(_.obs.newUserInputEnabled)
  val newUserDropdownEnabled    = *.focus("New user role dropdown enabled").value(_.obs.newUserDropdownEnabled)
  val leaveProjectButtonLoading = *.focus("Leave button loading").value(_.obs.leaveProjectButtonLoading)
  val existingUserRows          = *.focus("ExistingUser rows").collection(_.obs.existingUserRows.map(_.row))

  // ===================================================================================================================

  val clickAdd: *.Actions =
    *.action("Click 'Add' button")(_.obs.addButton.click())

  def setNewUserInput(usernameOrEmail: String): *.Actions =
    *.action(s"Set new user input to '$usernameOrEmail'")(_.obs.newUserInput.setValue(usernameOrEmail))

  def existingUserSelect(rowIdx: Int, role: ProjectRole): *.Actions =
    *.action(s"Select '$role' in dropdown in row $rowIdx")(_.obs.existingUserRows(rowIdx).dropdown.select(role.toString))

  def existingUserClickSave(rowIdx: Int): *.Actions =
    *.action(s"Click 'Save' button in row $rowIdx")(_.obs.existingUserRows(rowIdx).saveButton.click())

  def existingUserClickDelete(rowIdx: Int): *.Actions =
    *.action(s"Click 'Delete' button in row $rowIdx")(_.obs.existingUserRows(rowIdx).deleteButton.click())

  val clickLeaveProject: *.Actions =
    *.action("Click 'Leave This Project'")(_.obs.leaveProjectButton.click())
}
