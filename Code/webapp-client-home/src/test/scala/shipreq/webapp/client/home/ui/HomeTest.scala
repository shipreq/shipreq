package shipreq.webapp.client.home.ui

import japgolly.scalajs.react.test._
import java.time.Instant
import java.time.temporal.ChronoUnit._
import monocle.macros.Lenses
import org.scalajs.dom.html
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.TestAjaxClient
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.home.test.PrepareEnv
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.protocol.ajax.HomeSpaProtocols
import shipreq.webapp.member.protocol.entrypoint.HomeSpaEntryPoint
import shipreq.webapp.member.ui.BaseStyles
import utest._

final class HomeObs(cp: TestAjaxClient, $: DomZipperJs) {

  val reqs = cp.reqs.length

  val projectDoms = $.collect0n("." + BaseStyles.projectItems.item.className.value)

  val projectNames: Vector[String] =
    projectDoms.map(_("h1").innerText)

  object createProject {
    private val cont = $("." + Styles.createProjectCont.className.value)

    val input  = cont("input").domAs[html.Input]
    val button = cont("button").domAs[html.Button]
    val error  = cont.collect01(".ui.pointing.label").doms.map(_.textContent)

    val inputText      = input.value
    val inputDisabled  = input.disabled
    val buttonDisabled = button.disabled
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object HomeTestDsl {
  sealed abstract class CPState
  object CPState {
    case object Blank      extends CPState
    case object Ready      extends CPState
    case object InputError extends CPState
    case object Locked     extends CPState
    case object AsyncError extends CPState
  }

  @Lenses
  case class State(cpText: String, cpState: CPState, projects: Vector[String], reqs: Int)

  val clearCP = State.cpText.replace("") compose State.cpState.replace(CPState.Blank)

  val * = Dsl[TestAjaxClient, HomeObs, State]

  private def cpState(inputDisabled: Boolean, buttonDisabled: Boolean, hasError: Boolean) =
    *.focus("CreateProject input disabled") .value(_.obs.createProject.inputDisabled  ).assert(inputDisabled) &
    *.focus("CreateProject button disabled").value(_.obs.createProject.buttonDisabled ).assert(buttonDisabled) &
    *.focus("CreateProject has error")      .value(_.obs.createProject.error.isDefined).assert(hasError)

  val invariants: *.Invariants = (
    *.focus("Project names").obsAndState(_.projectNames, _.projects).map(_.sorted).assert.equal &
    *.focus("CreateProject text").obsAndState(_.createProject.inputText, _.cpText).assert.equal &
    *.chooseInvariant("CreateProject state")(_.state.cpState match {
      case CPState.Blank      => cpState(false, true , false)
      case CPState.Ready      => cpState(false, false, false)
      case CPState.InputError => cpState(false, true , true )
      case CPState.Locked     => cpState(true , true , false)
      case CPState.AsyncError => cpState(false, false, true )
    }) &
    *.focus("AJAX requests").obsAndState(_.reqs, _.reqs).assert.equal
  )

  def setCPText(text: String): *.Actions =
    *.action(s"Set CreateProject text to [$text]")(SimEvent.Change(text) simulate _.obs.createProject.input)
      .updateState(_.copy(cpText = text))

  def setCPText(text: String, newState: CPState): *.Actions =
    setCPText(text).updateState(_.copy(cpState = newState))

  val clickCreateProject =
    *.action("Click CreateProject")(Simulate click _.obs.createProject.button)

  val reqCreateProject =
    clickCreateProject.updateState(State.reqs.modify(_ + 1) compose State.cpState.replace(CPState.Locked))

  val ajaxFailLast =
    *.action("Simulate AJAX error")(_.ref.failLast())

  def ajaxCreatedProject(p: ProjectMetaData) =
    *.action("Simulate project-creation AJAX")(_.ref.respondToLast(HomeSpaProtocols.CreateProject.ajax)(p))
      .updateState(State.projects.modify(_ :+ p.name) compose clearCP)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object HomeTest extends TestSuite {
  import HomeTestDsl._

  PrepareEnv()

  def run(ps: List[ProjectMetaData])(plan: *.Plan): Report[String] = {
    val cp = new TestAjaxClient(false)
    val init = HomeSpaEntryPoint.InitData(Username("thatguy"), ps, AssetManifest(None))
    val props = Home.Props(init, cp)
    ReactTestUtils.withRenderedIntoDocument(props.render)(c =>
      plan
        .addInvariants(invariants)
        .withInitialState(State("", CPState.Blank, ps.iterator.map(_.name).toVector, 0))
        .test(Observer(new HomeObs(_, c.domZipper)))
        .withRef(cp)
        .run()
    )
  }

  object Data {
    import shipreq.webapp.member.test.project.UnsafeTypes._
    import ProjectRole._
    val now = Instant.now()
    val piE = ProjectMetaData("abeF", "Empty", Admin, 0, 0, 0, 0, now.minus(18, DAYS), now.minus(19, DAYS), None)
    val piO = ProjectMetaData("qwe3F", "Old", Admin, 2, 1581, 311, 340, now.minus(92, DAYS), now.minus(7, MINUTES), Some(now.minus(7, MINUTES)))
    val piN = ProjectMetaData("wenkj", "New", Admin, 2, 2, 0, 0, now, now, None)
    val pc  = List(piE, piO)
  }

  override def tests = Tests {
    "createProject" - run(Data.pc)(Plan.action(
      setCPText("    ")
        >> setCPText("Oh and I see and I know, and suddenly I'm on my own, but it's now and it's no, at least it isn't tomorrow. " * 3, CPState.InputError)
        >> setCPText("  ahhhh ness  ", CPState.Ready)
        >> reqCreateProject >> ajaxFailLast.updateState(State.cpState replace CPState.AsyncError)
        >> reqCreateProject.rename("Retry") >> ajaxFailLast.updateState(State.cpState replace CPState.AsyncError)
        >> setCPText("  ahh ness  ", CPState.Ready) // AJAX failure ---[edit]---> Ready
        >> reqCreateProject >> ajaxCreatedProject(Data.piN)
    )).assert()
  }
}
