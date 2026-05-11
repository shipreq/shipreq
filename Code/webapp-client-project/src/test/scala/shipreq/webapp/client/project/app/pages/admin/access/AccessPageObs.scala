package shipreq.webapp.client.project.app.pages.admin.access

import org.scalajs.dom.html
import shipreq.base.util.Enabled
import shipreq.webapp.base.data.ProjectPerm
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.test._
import shipreq.webapp.member.test.CommonObs

object AccessPageObs {

  def parsePerm(perm: String): ProjectPerm =
    perm match {
      case "Admin"        => ProjectPerm.Admin
      case "Collaborator" => ProjectPerm.Collaborator
      case _              => sys.error(s"Unknown perm '$perm'")
    }

  final class ExistingUserRowObs($: DomZipperJs) {
    val name               = $("td", 1 of 4).domAsHtml.textContent.trim
    val dropdown           = new CommonObs.Dropdown($("td", 2 of 4))
    val dropdownEnabled    = Enabled.unless(dropdown.isDisabled)
    val selectedPerm       = parsePerm(dropdown.selected.get)
    val saveButtonZipper   = $("td", 3 of 4)("button")
    val saveButton         = saveButtonZipper.domAsHtml
    val saveButtonStatus   = ButtonStatus(saveButtonZipper)
    val deleteButtonZipper = $("td", 4 of 4)("button")
    val deleteButton       = deleteButtonZipper.domAsHtml
    val deleteButtonStatus = ButtonStatus(deleteButtonZipper)
    val row                = ExistingUserRow(name, selectedPerm, dropdownEnabled, saveButtonStatus, deleteButtonStatus)
  }

  sealed trait ButtonStatus
  object ButtonStatus {
    case object Enabled extends ButtonStatus
    case object Disabled extends ButtonStatus
    case object Loading extends ButtonStatus

    def apply(button: DomZipperJs): Option[ButtonStatus] = {
      val dom = button.domAsHtml
      if (dom.style.visibility == "hidden")
        None
      else if (dom.classList.contains("loading"))
        Some(Loading)
      else if (dom.classList.contains("disabled") || dom.hasAttribute("disabled"))
        Some(Disabled)
      else
        Some(Enabled)
    }

    implicit def univEq: UnivEq[ButtonStatus] = UnivEq.derive
  }

  final case class ExistingUserRow(name        : String,
                                   perm        : ProjectPerm,
                                   dropdown    : Enabled,
                                   saveButton  : Option[ButtonStatus],
                                   deleteButton: Option[ButtonStatus])

  object ExistingUserRow {
    implicit def univEq: UnivEq[ExistingUserRow] = UnivEq.derive
  }
}

final class AccessPageObs($: DomZipperJs, val global: TestGlobal.Obs, val confirmJs: TestConfirmJs.Obs) {
  import AccessPageObs._

  private lazy val newUserSegment = $.child(".segment", 1 of 3)
  lazy val newUserInput           = new CommonObs.Input(newUserSegment("input:text"))
  lazy val newUserInputField      = $(".field", 1 of 3).domAsHtml
  lazy val newUserInputEnabled    = Enabled.unless(newUserInputField.classList.contains("disabled"))
  lazy val newUserDropdown        = new CommonObs.Dropdown(newUserSegment(".ui.dropdown"))
  lazy val newUserDropdownEnabled = Enabled.unless(newUserDropdown.isDisabled)
  lazy val addButtonZipper        = newUserSegment("button")
  lazy val addButton              = addButtonZipper.domAs[html.Button]
  lazy val addButtonStatus        = ButtonStatus(addButtonZipper).get

  private lazy val existingUserSegment = $.child(".segment", 2 of 3)
  lazy val existingUserRows = existingUserSegment.collect1n("tr").map(new ExistingUserRowObs(_))

  private lazy val leaveProjectSegment = $.child(".segment", 3 of 3)
  lazy val leaveProjectButton          = leaveProjectSegment("button").domAs[html.Button]
  lazy val leaveProjectButtonLoading   = leaveProjectButton.classList.contains("loading")
}
