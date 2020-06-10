package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react.test._
import japgolly.univeq._
import org.scalajs.dom.html
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test._
import utest._

class ProjectHomeObs($: DomZipperJs) {
  private val projectArea = $(">section", 1 of 2)

  val projectNameViewDom: Option[html.Element] =
    projectArea.collect01("h1").domsAsHtml

  val projectNameView: Option[String] =
    projectNameViewDom.map(_.textContent)

  val projectNameEditInput: Option[html.Input] =
    projectArea.collect01("input:text").domsAs[html.Input]

  val projectNameEditValue: Option[String] =
    projectNameEditInput.map(_.value)

  val projectNameEditDisabled: Option[Boolean] =
    projectNameEditInput.map(_.disabled)

  val projectNameErrMsg: Option[String] =
    projectArea.collect01(".ui.label").innerTexts

  val changes: Int =
    projectArea(".statistic:contains('change') .value").innerText.trim.toInt

  val reqCount: Int =
    projectArea(".statistic:contains('req') .value").innerText.trim.toInt
}

// =====================================================================================================================

object ProjectHomeTestDsl {

  val * = Dsl[TestGlobal, ProjectHomeObs, Project]

  val invariants: *.Invariants =
    *.focus("Req count").obsAndState(_.reqCount, _.liveReqCount).assert.equal &
    *.focus("Changes").value(_.obs.changes).test(_ + " must be positive.")(_ >= 0) &
    *.chooseInvariant("Project name")(_.obs.projectNameView match {
      case Some(name) => *.focus("Project name").value(_ => name).assert.equalBy(_.state.name)
      case None       => *.emptyInvariant
    }) &
    *.test("Project name mode = view XOR edit")(x => x.obs.projectNameView.isEmpty !=* x.obs.projectNameEditInput.isEmpty)

  val editorOpen =
    *.focus("Editor.open?").value(_.obs.projectNameEditInput.isDefined)

  val editorDisabled =
    *.focus("Editor.disabled?").value(_.obs.projectNameEditDisabled.get)

  val editValue =
    *.focus("Editor.value").value(_.obs.projectNameEditValue.get)

  val editHasError =
    *.focus("Editor.hasError?").value(_.obs.projectNameErrMsg.isDefined)

  val startEdit =
    *.action("Start editing project name")(Simulate doubleClick _.obs.projectNameViewDom.get) +>
    editorOpen.assert.beforeAndAfter(false, true) +>
    editHasError.assert(false) +>
    editValue.assert.equalBy(_.state.name) +>
    editorDisabled.assert(false)

  val abortEdit =
    *.action("Hit Escape")(KB.Escape simulateKeyDownUp _.obs.projectNameEditInput.get) +>
    editorOpen.assert.beforeAndAfter(true, false) +>
    editHasError.assert(false)

  val commitEdit =
    *.action("Hit Ctrl-Enter")(KB.Enter.ctrl simulateKeyDownUp _.obs.projectNameEditInput.get)

  def setEditValue(text: String) =
    editorOpen.assert(true) +>
    *.action("Edit project name: " + text.display)(SimEvent.Change(text) simulate _.obs.projectNameEditInput.get) +>
    editValue.assert(text)

  val serverDontAutoRespond =
    *.action("Prevent server auto-responding")(_.ref.disableAutoResponse())

  val serverAutoRespondToLast =
    *.action("Server responds")(_.ref.autoRespondToLast())
}

// =====================================================================================================================

object ProjectHomeTest extends TestSuite {
  import ProjectHomeTestDsl._

  PrepareEnv()

  def projectNameEditing: *.Actions = (
    serverDontAutoRespond
    >> startEdit
    >> setEditValue("") +> editHasError.assert(true)
    >> commitEdit +> editValue.assert.noChange
    >> setEditValue("ok fine") +> editHasError.assert(false)
    >> abortEdit
    >> startEdit >> commitEdit +> editorOpen.assert(false)
    >> startEdit >> setEditValue("  omg fine  ")
    >> commitEdit +> editorDisabled.assert(true)
    >> serverAutoRespondToLast.updateState(_.copy(name = "omg fine")) +> editorOpen.assert(false)
    >> startEdit >> abortEdit
  )

  def run(actions: *.Actions): Unit = {
    import ProjectSpaTestDsl._
    runTest(actions.lift, page = Page.Index)
  }

  override def tests = Tests {
    "projectNameEditing" - run(projectNameEditing)
  }
}
