package shipreq.webapp.client.app

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import scala.util.{Failure, Success, Try}
import shipreq.base.util.Debug._
import shipreq.webapp.base.data.{ExternalPubid, Project}
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.{MockRemotes, SampleProject5}
import shipreq.webapp.client.app.reqdetail.{ReqDetailObs, ReqDetailTestDsl => RD}
import shipreq.webapp.client.app.reqtable.{ReqTableObs, ReqTableTestDsl => RT}
import shipreq.webapp.client.test._
import DomZipper.Implicits._
import ProjectSpaMain.{Page, Props, State}
import TestState._

object ProjectSpaTestDsl {

  type Maybe[A] = Either[String, A]

  implicit def tryToEither[A](t: Try[A]): Maybe[A] =
    t match {
      case Success(s) => Right(s)
      case Failure(f) => Left("Try failed: " + f.toString)
    }

  case class Ref(cd: TestClientData, svr: MockServer, tester: ComponentTester[Props, State, _, TopNode]) {
    def observe(): Obs = {
      def inner = DomZipper(tester.component).down(">*", 2 of 2)
      new Obs(
        cd.project(),
        Try(new ReqTableObs(svr, inner)),
        Try(new ReqDetailObs(inner)))
    }
  }

  case class Obs(project  : Project,
                 reqTable : Maybe[ReqTableObs],
                 reqDetail: Maybe[ReqDetailObs])

  @Lenses
  case class TestState(page: Page, project: Project, detailState: RD.State)

  val * = Dsl[Ref, Obs, TestState]

  implicit val transformRT =
    RT.*.transformer
      .mapR[Ref](r => RT.Ref(r.tester.component zoomL State.reqTable, r.svr))
      .pmapO[Obs](_.reqTable)
      .mapS(TestState.project.get)((a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support

  implicit val transformRD =
    RD.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.reqDetail)
      .mapS[TestState](s => RD.TestState(s.project, s.detailState))((s, d) => TestState(s.page, d.project, d.state))

  val invariantsRT = RT.invariants.lift
  val invariantsRD = RD.invariants.lift

  val invariants: *.Invariant =
    *.chooseInvariant("Page invariants")(_.state.page match {
      case Page.ReqTable     => invariantsRT
      case Page.ReqDetail(_) => invariantsRD
      case _                 => *.emptyInvariant
    })

  def setPage(p: Page): *.Action = p match {
    case Page.ReqDetail(_) => sys error "Use setPageToReqDetail instead."
    case _                 => _setPage(p)
  }

  private def _setPage(p: Page): *.Action =
    *.action(s"Set page to $p")(_.ref.tester.modProps(_.copy(page = p)))
      .updateState(_.copy(page = p))

  def setPageToReqDetail(ep: ExternalPubid, mode: RD.Mode): *.Action =
    _setPage(Page.ReqDetail(ep)).updateState(_.copy(detailState = RD.State(ep, mode)))

  def applyEvents(e: Event*): *.Action =
    applyEvents(e.mkString("Apply events: ", ", ", "."), e: _*)

  def applyEvents(name: => String, e: Event*): *.Action =
    *.action(name)(_.ref.cd.applyTestEvents(e: _*))
      .updateStateBy(i => i.state.copy(project = i.obs.project))

  def liftReqTableTests (p: RT.*.Plan): *.Plan = p.lift
  def liftReqDetailTests(p: RD.*.Plan): *.Plan = p.lift

  def testReqTable(action: RT.*.Action): *.Action =
    liftReqTableTests(Plan.action(action)).asAction("Test ReqTable")

  def testReqDetail(action: RD.*.Action): *.Action =
    liftReqDetailTests(Plan.action(action)).asAction("Test ReqDetail")

  def runTest(action : *.Action,
              project: Project  = SampleProject5.project,
              page   : Page     = Page.ReqTable,
              rd     : RD.State = RD.unspecifiedState)
            : Unit = {
    val cd   = TestClientData(project)
    val svr  = MockServer(cd)
    val spa  = new ProjectSpaMain(MockRemotes.projectSPA, svr, cd)
    val rc   = MockRouterCtl[Page]()
    val init = TestState(page, cd.project(), rd)

    ComponentTester(spa.Component)(Props(init.page, rc)) { tester =>
      val tt  = Plan(action, invariants).test(Observer(_.observe()))
      val r   = tt.run(init, Ref(cd, svr, tester))
      if (r.failed)
        println(s"${"="*120}\n${removeReactIds(tester.component.getDOMNode().outerHTML)}\n")
      r.assert()
    }
  }

}