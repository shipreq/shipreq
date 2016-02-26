package shipreq.webapp.client.app

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import scala.util.{Failure, Success, Try}
import teststate._
import shipreq.webapp.base.data.{ExternalPubid, Project}
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.{MockRemotes, SampleProject3}
import shipreq.webapp.client.app.reqdetail.{ReqDetailObs, ReqDetailTestDsl => RD}
import shipreq.webapp.client.app.reqtable.{ReqTableObs, ReqTableTestDsl => RT}
import shipreq.webapp.client.test._
import DomZipper.Implicits._
import ProjectSpaMain.{Page, Props, State}

object ProjectSpaTestDsl {

  type Maybe[A] = Either[String, A]

  implicit def tryToEither[A](t: Try[A]): Maybe[A] =
    t match {
      case Success(s) => Right(s)
      case Failure(f) => Left(f.toString)
    }

  case class Ref(cd: TestClientData, tester: ComponentTester[Props, State, _, TopNode]) {
    def observe(): Obs = {
      def inner = DomZipper(tester.component).down(">*", 2 of 2)
      new Obs(
        cd.project(),
        Try(new ReqDetailObs(inner)),
        Try(new ReqTableObs(inner)))
    }
  }

  case class Obs(project  : Project,
                 reqDetail: Maybe[ReqDetailObs],
                 reqTable : Maybe[ReqTableObs])

  @Lenses
  case class TestState(page: Page, project: Project, expectedError: Option[String])

  val * = Dsl.sync[Ref, Obs, TestState, String]

  val invariants: *.Check =
    Check.empty

  def setPage(p: Page): *.Action =
    *.action(s"Set page to $p")
      .updateState(_.copy(page = p))
      .act(_.ref.tester.modProps(_.copy(page = p)))

  def setPageToReqDetail(ep: ExternalPubid, expectedError: Option[String]): *.Action =
    setPage(Page.ReqDetail(ep)).modS(_.copy(expectedError = expectedError))

  def applyEvents(e: Event*): *.Action =
    applyEvents(e.mkString("Apply events: ", ", ", "."), e: _*)

  def applyEvents(name: => String, e: Event*): *.Action =
    *.action(name)
      .updateStateO(s => o => s.copy(project = o.project))
      .act(_.ref.cd.applyTestEvents(e: _*))

  def testReqTable(action: RT.*.Action = Action.empty): *.Action =
    liftReqTableTests(RT(action)).asAction("Test ReqTable")

  def liftReqTableTests(tc: RT.*.TestContent): *.TestContent =
    tc.cmapR[Ref](_.tester.component zoomL State.reqTable)
      .pmapO[Obs](_.reqTable)
      .cmapS[TestState](TestState.project.get, (a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support

  def liftReqDetailTests(tc: RD.*.TestContent): *.TestContent =
    tc.cmapR[Ref](_ => ())
      .pmapO[Obs](_.reqDetail)
      .cmapS[TestState](
        s => RD.TestState(s.project, s.expectedError),
        (s, d) => s.copy(s.page, d.project, d.expectedError))

  def runTest(action: *.Action): Unit = {
    val cp   = new TestClientProtocol
    val cd   = TestClientData(SampleProject3.project)
    val spa  = new ProjectSpaMain(MockRemotes.projectSPA, cp, cd)
    val rc   = MockRouterCtl[Page]()
    val init = TestState(Page.ReqTable, cd.project(), Some("init"))

    ComponentTester(spa.Component)(Props(init.page, rc)) { tester =>
      val tt  = Test(action, invariants).observe(_.observe())
      val h   = tt.run(init, Ref(cd, tester))
      // println(h.format(History.Options.colored.alwaysShowChildren))
      h.assert(History.Options.colored)
      // println(h.format(History.Options.colored))
    }
  }

}