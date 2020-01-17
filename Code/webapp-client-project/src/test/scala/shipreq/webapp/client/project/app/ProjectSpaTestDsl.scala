package shipreq.webapp.client.project.app

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import japgolly.univeq._
import monocle.macros.Lenses
import scala.util.{Failure, Success, Try}
import teststate.run.Report.AssertionSettings
import shipreq.base.util.Debug._
import shipreq.webapp.base.data.{ExternalPubid, Obfuscated, Project}
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.protocol.ProjectSpaEntryPoint
import shipreq.webapp.base.test.SampleProject5
import shipreq.webapp.base.test._
import shipreq.webapp.base.user.Username
import shipreq.webapp.client.project.app.cfg.reqtypes.{CfgReqTypesObs, CfgReqTypesDsl => CRT}
import shipreq.webapp.client.project.app.issues.{IssuesPageObs, IssuesPageTestDsl => IP}
import shipreq.webapp.client.project.app.reqdetail.{ReqDetailObs, ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.app.reqtable.{ReqTableObs, ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.root.{ProjectHomeTestDsl => PH, _}
import shipreq.webapp.client.project.test._
import LoadedRoot.Props
import Routes.Page
import TestState._

object ProjectSpaTestDsl {

  type Maybe[+A] = Either[String, A]

  implicit def tryToEither[A](t: Try[A]): Maybe[A] =
    t match {
      case Success(s) => Right(s)
      case Failure(f) => Left("Try failed: " + f.toString)
    }

  class ComponentTester[P, S, B](val c: ScalaComponent[P, S, B, CtorType.Props])(init: ScalaComponent.MountedImpure[P, S, B]) {
    var component = init
    def modProps(f: P => P): Unit =
      component = ReactTestUtils.modifyProps(c, component)(f)
  }

  case class Ref(global: TestGlobal, tester: ComponentTester[Props, State, _]) {
    def observe(): Obs = {
      val $ = tester.component.domZipper
      val inner = $(">div")(">div,>main")
      val nav = new NavObs($(">nav"), inner)

      val empty: Obs = {
        val e = Left("Chosen page is: " + nav.page)
        Obs(global.unsafeProject(), nav, e, e, e, e, e)
      }

      nav.page match {
        case Page.Index        => empty.copy(home        = Try(new ProjectHomeObs(inner)))
        case Page.CfgReqTypes  => empty.copy(cfgReqTypes = Try(new CfgReqTypesObs(inner)))
        case Page.ReqTable     => empty.copy(reqTable    = Try(new ReqTableObs(global, inner)))
        case Page.ReqDetail(_) => empty.copy(reqDetail   = Try(new ReqDetailObs(inner)))
        case Page.Issues       => empty.copy(issues      = Try(new IssuesPageObs(inner)))
        case _                 => empty
      }
    }
  }

  class NavObs(nav: DomZipperJs, inner: DomZipperJs) {
    val breadcrumbs = nav(".ui.breadcrumb").collect0n(".section")
    // println(nav.innerHTML)

    val projectName: String =
      breadcrumbs.doms(1).textContent

    val dropdownCrumbName: Option[String] =
      nav.collect01(".ui.dropdown.inline").doms.map { d =>
        // Not sure why this is needed
        val innerText = d.asInstanceOf[scalajs.js.Dynamic].innerText.asInstanceOf[String]
        val selected = innerText.takeWhile(_ != '\n')
        //println(s"[$innerText]")
        //println(s"[$selected]")
        selected
      }

    val page: Page =
      dropdownCrumbName match {
        case Some("Req Table") => Page.ReqTable
        case Some("Content")   => Page.ReqDetail(ExternalPubid.parse(breadcrumbs.zippers.last.innerText.trim).get)
        case Some("Fields")    => Page.CfgFields
        case Some("Req Types") => Page.CfgReqTypes
        case Some("Tags")      => Page.CfgTags
        case None              => Page.Index
        case Some("Issues")    => if (inner.exists(Style.issues.newIssueCont.selector)) Page.Issues else Page.CfgIssues
        case Some(n)           => sys error s"Unknown page: $n"
      }

    val unsavedChanges: Int =
      nav.collect01(".icon.edit").zippers.fold(0)(_.parent("span").innerText.toInt)
  }

  case class Obs(project    : Project,
                 nav        : NavObs,
                 home       : Maybe[ProjectHomeObs],
                 cfgReqTypes: Maybe[CfgReqTypesObs],
                 issues     : Maybe[IssuesPageObs],
                 reqTable   : Maybe[ReqTableObs],
                 reqDetail  : Maybe[ReqDetailObs])

  @Lenses
  case class TestState(page: Page, project: Project, detailState: RD.State)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val * = Dsl[Ref, Obs, TestState]

  implicit lazy val transformPH =
    PH.*.transformer
      .mapR[Ref](_.global)
      .pmapO[Obs](_.home)
      .mapS(TestState.project.get)((a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support

  implicit lazy val transformCRT =
    CRT.dsl.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.cfgReqTypes)
      .mapS[TestState](_ => ())((s, _) => s)

  implicit lazy val transformRT =
    RT.*.transformer
      .mapR[Ref](r => RT.Ref(r.tester.component zoomStateL State.reqTable, r.global))
      .pmapO[Obs](_.reqTable)
      .mapS(TestState.project.get)((a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support

  implicit lazy val transformRD =
    RD.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.reqDetail)
      .mapS[TestState](s => RD.TestState(s.project, s.detailState))((s, d) => TestState(s.page, d.project, d.state))

  implicit lazy val transformIP =
    IP.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.issues)
      .mapS[TestState](_ => ())((s, _) => s)

  private lazy val invariantsPH = PH.invariants.lift
  private lazy val invariantsRT = RT.invariants.lift
  private lazy val invariantsRD = RD.invariants.lift
  private lazy val invariantsIP = IP.invariants.lift

  private val pageInvariants: *.Invariants =
    *.chooseInvariant("Page invariants")(_.state.page match {
      case Page.Index        => invariantsPH
      case Page.ReqTable     => invariantsRT
      case Page.ReqDetail(_) => invariantsRD
      case Page.Issues       => invariantsIP
      case _                 => *.emptyInvariant
    })

  private val invariants: *.Invariants =
    pageInvariants.when(i => i.obs.nav.page ==* i.state.page) &
    *.focus("Page").obsAndState(_.nav.page, _.page).assert.equal &
    *.focus("Project name in NavBar").obsAndState(_.nav.projectName, _.project.name).assert.equal

  val unsavedChanges = *.focus("unsaved changes").value(_.obs.nav.unsavedChanges)

  def setPage(p: Page): *.Actions = p match {
    case Page.ReqDetail(_) => sys error "Use setPageToReqDetail instead."
    case _                 => _setPage(p)
  }

  private def _setPage(p: Page): *.Actions =
    *.action(s"Set page to $p")(_.ref.tester.modProps(_.copy(page = p)))
      .updateState(_.copy(page = p))

  def setPageToReqDetail(ep: ExternalPubid, mode: RD.Mode): *.Actions =
    _setPage(Page.ReqDetail(ep)).updateState(_.copy(detailState = RD.State(ep, mode)))

  def applyEvents(e: Event*): *.Actions =
    applyEvents(e.mkString("Apply events: ", ", ", "."), e: _*)

  def applyEvents(name: => String, es: Event*): *.Actions =
    *.action(name)(_.ref.global.applyTestEventsCB(es: _*).runNow())
      .updateStateBy(i => i.state.copy(project = i.obs.project))

  def liftProjectHomeTests(p: PH.*.Plan): *.Plan = p.lift
  def liftReqTableTests   (p: RT.*.Plan): *.Plan = p.lift
  def liftReqDetailTests  (p: RD.*.Plan): *.Plan = p.lift
  def liftIssuePageTests  (p: IP.*.Plan): *.Plan = p.lift

  def testReqTable(action: RT.*.Actions): *.Actions =
    liftReqTableTests(Plan.action(action)).asAction("Test ReqTable")

  def testReqDetail(action: RD.*.Actions): *.Actions =
    liftReqDetailTests(Plan.action(action)).asAction("Test ReqDetail")

  def runTest(action : *.Actions,
              page   : Page,
              project: Project  = SampleProject5.project,
              rd     : RD.State = RD.unspecifiedState): Unit = {

    val global       = TestGlobal(project)
    val initPageData = ProjectSpaEntryPoint.InitData(Username("testuser"), Obfuscated("xyz"), project.name)
    val spa          = new LoadedRoot(initPageData, global)
    val rc           = MockRouterCtl[Page]()
    val init         = TestState(page, global.unsafeProject(), rd)

    ReactTestUtils.withRenderedIntoBody(spa.Component(Props(init.page, rc))) { m =>
      val tester = new ComponentTester(spa.Component)(m)
      val report = Plan(action, invariants)
                     .test(Observer(_.observe()))
                     .withInitialState(init)
                     .withRefByName(Ref(global, tester))
                     .run()
      assertTestState(report)
//      assertTestState(r, println(s"${"=" * 120}\n${htmlScrub run tester.component.getDOMNode.map(_.asElement).outerHTML}\n"))
    }
  }

}