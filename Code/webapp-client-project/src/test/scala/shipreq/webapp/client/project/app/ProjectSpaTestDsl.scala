package shipreq.webapp.client.project.app

import japgolly.microlibs.testutil.TestUtil
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import scala.util.{Failure, Success, Try}
import teststate.run.Report.AssertionSettings
import shipreq.base.util.Debug._
import shipreq.webapp.base.data.{ExternalPubid, Project}
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.{MockRemotes, SampleProject5}
import shipreq.webapp.client.base.test._
import shipreq.webapp.client.project.app.reqdetail.{ReqDetailObs, ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.app.reqtable2.{ReqTableObs, ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.root.{ProjectHomeTestDsl => PH, _}
import shipreq.webapp.client.project.test._
import LoadedRoot.Props
import Routes.Page
import TestState._

object ProjectSpaTestDsl {

  type Maybe[A] = Either[String, A]

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

  case class Ref(cd: TestClientData, svr: MockServer, tester: ComponentTester[Props, State, _]) {
    def observe(): Obs = {
      val $ = tester.component.htmlDomZipper
      def inner = $(">*", 2 of 2) // navBar & body
      new Obs(
        cd.project(),
        new NavObs($("nav:contains('Logout')")),
        Try(new ProjectHomeObs(inner)),
        Try(new ReqTableObs(svr, inner)),
        Try(new ReqDetailObs(inner)))
    }
  }

  class NavObs(nav: HtmlDomZipper) {
    val breadcrumbs = nav(".ui.breadcrumb").collect0n(".section")

    val projectName: String =
      breadcrumbs.doms(1).textContent
  }

  case class Obs(project  : Project,
                 nav      : NavObs,
                 home     : Maybe[ProjectHomeObs],
                 reqTable : Maybe[ReqTableObs],
                 reqDetail: Maybe[ReqDetailObs])

  @Lenses
  case class TestState(page: Page, project: Project, detailState: RD.State)

  val * = Dsl[Ref, Obs, TestState]

  implicit lazy val transformPH =
    PH.*.transformer
      .mapR[Ref](_.svr)
      .pmapO[Obs](_.home)
      .mapS(TestState.project.get)((a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support

  implicit lazy val transformRT =
    RT.*.transformer
      .mapR[Ref](r => RT.Ref(r.tester.component zoomStateL State.reqTable, r.svr))
      .pmapO[Obs](_.reqTable)
      .mapS(TestState.project.get)((a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support

  implicit lazy val transformRD =
    RD.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.reqDetail)
      .mapS[TestState](s => RD.TestState(s.project, s.detailState))((s, d) => TestState(s.page, d.project, d.state))

  private lazy val invariantsPH = PH.invariants.lift
  private lazy val invariantsRT = RT.invariants.lift
  private lazy val invariantsRD = RD.invariants.lift

  private val pageInvariants: *.Invariants =
    *.chooseInvariant("Page invariants")(_.state.page match {
      case Page.Index        => invariantsPH
      case Page.ReqTable     => invariantsRT
      case Page.ReqDetail(_) => invariantsRD
      case _                 => *.emptyInvariant
    })

  private val invariants: *.Invariants =
    pageInvariants &
    *.focus("Project name in NavBar").obsAndState(_.nav.projectName, _.project.name).assert.equal

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

  def applyEvents(name: => String, e: Event*): *.Actions =
    *.action(name)(_.ref.cd.applyTestEvents(e: _*))
      .updateStateBy(i => i.state.copy(project = i.obs.project))

  def liftProjectHomeTests(p: PH.*.Plan): *.Plan = p.lift
  def liftReqTableTests   (p: RT.*.Plan): *.Plan = p.lift
  def liftReqDetailTests  (p: RD.*.Plan): *.Plan = p.lift

  def testReqTable(action: RT.*.Actions): *.Actions =
    liftReqTableTests(Plan.action(action)).asAction("Test ReqTable")

  def testReqDetail(action: RD.*.Actions): *.Actions =
    liftReqDetailTests(Plan.action(action)).asAction("Test ReqDetail")

  def runTest(action : *.Actions,
              page   : Page,
              project: Project  = SampleProject5.project,
              rd     : RD.State = RD.unspecifiedState): Unit = {
    val cd   = TestClientData(project)
    val svr  = MockServer(cd)
    val spa  = new LoadedRoot(MockRemotes.projectSpa(project), svr, cd)
    val rc   = MockRouterCtl[Page]()
    val init = TestState(page, cd.project(), rd)

    ReactTestUtils.withRenderedIntoBody(spa.Component(Props(init.page, rc))) { m =>
      val tester = new ComponentTester(spa.Component)(m)
      val tt  = Plan(action, invariants).test(Observer(_.observe()))
      val r   = tt.run(init, Ref(cd, svr, tester))
      assertTestState(r)
//      assertTestState(r, println(s"${"=" * 120}\n${htmlScrub run tester.component.getDOMNode.outerHTML}\n"))
    }
  }

}