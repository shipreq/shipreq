package shipreq.webapp.client.app

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import scala.util.{Failure, Success, Try}
import teststate.Exports._
import shipreq.base.util.Debug._
import shipreq.webapp.base.data.{ExternalPubid, Project}
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.{MockRemotes, SampleProject5}
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
      case Failure(f) => Left("Try failed: " + f.toString)
    }

  case class Ref(cd: TestClientData, tester: ComponentTester[Props, State, _, TopNode]) {
    def observe(): Obs = {
      def inner = DomZipper(tester.component).down(">*", 2 of 2)
      new Obs(
        cd.project(),
        Try(new ReqTableObs(inner)),
        Try(new ReqDetailObs(inner)))
    }
  }

  case class Obs(project  : Project,
                 reqTable : Maybe[ReqTableObs],
                 reqDetail: Maybe[ReqDetailObs])

  @Lenses
  case class TestState(page: Page, project: Project, detailState: RD.State)

  val * = Dsl.sync[Ref, Obs, TestState, String]

  implicit val transformRT =
    RT.*.transformer
      .mapR[Ref](_.tester.component zoomL State.reqTable)
      .pmapO[Obs](_.reqTable)
      .mapS(TestState.project.get)((a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support

  implicit val transformRD =
    RD.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.reqDetail)
      .mapS[TestState](s => RD.TestState(s.project, s.detailState))((s, d) => s.copy(s.page, d.project, d.ep))

  val invariantsRT = RT.invariants.lift
  val invariantsRD = RD.invariants.lift

  val invariants: *.Invariant =
    *.chooseInvariant("Page invariants", _.state.page match {
      case Page.ReqTable     => invariantsRT
      case Page.ReqDetail(_) => invariantsRD
      case _                 => emptyInvariants
    })

  def setPage(p: Page): *.Action = p match {
    case Page.ReqDetail(_) => sys error "Use setPageToReqDetail instead."
    case _                 => _setPage(p)
  }

  private def _setPage(p: Page): *.Action =
    *.action(s"Set page to $p")
      .act(_.ref.tester.modProps(_.copy(page = p)))
      .updateState(_.copy(page = p))

  def setPageToReqDetail(ep: ExternalPubid, detailState: RD.State): *.Action =
    _setPage(Page.ReqDetail(ep)).updateState(_.copy(detailState = detailState))

  def applyEvents(e: Event*): *.Action =
    applyEvents(e.mkString("Apply events: ", ", ", "."), e: _*)

  def applyEvents(name: => String, e: Event*): *.Action =
    *.action(name)
      .act(_.ref.cd.applyTestEvents(e: _*))
      .updateStateBy(i => i.state.copy(project = i.obs.project))

  def liftReqTableTests(tc: RT.*.TestContent): *.TestContent = tc.lift
  def liftReqDetailTests(tc: RD.*.TestContent): *.TestContent = tc.lift

  def testReqTable(action: RT.*.Action): *.Action = liftReqTableTests(Test(action)).asAction("Test ReqTable")
  def testReqDetail(action: RD.*.Action): *.Action = liftReqDetailTests(Test(action)).asAction("Test ReqDetail")

  def runTest(action: *.Action,
              project: Project  = SampleProject5.project,
              page   : Page     = Page.ReqTable,
              rd     : RD.State = None)
            : Unit = {
    val cd   = TestClientData(project)
    val cp   = MockServer(cd)
    val spa  = new ProjectSpaMain(MockRemotes.projectSPA, cp, cd)
    val rc   = MockRouterCtl[Page]()
    val init = TestState(page, cd.project(), rd)

    ComponentTester(spa.Component)(Props(init.page, rc)) { tester =>
      val tt  = Test(action, invariants).observe(_.observe())
      val h   = tt.run(init, Ref(cd, tester))
      h.assert(History.Options.colored.alwaysShowChildren)
      // println(h.format(History.Options.colored.alwaysShowChildren))
    }
  }

}