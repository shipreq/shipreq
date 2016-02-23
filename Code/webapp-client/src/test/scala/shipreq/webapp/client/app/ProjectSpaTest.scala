package shipreq.webapp.client.app

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import scala.util.{Failure, Success, Try}
import teststate._
import utest._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Delete, DeleteCustomField, Event}
import shipreq.webapp.base.test.SampleProject.Values.priField
import shipreq.webapp.base.test._
import shipreq.webapp.client.app.reqtable.{ReqTableObs, ReqTableTestDsl => RT}
import shipreq.webapp.client.test._
import DomZipper.Implicits._
import ProjectSpaMain.{Page, Props, State}

object ProjectSpaDsl {

  case class Ref(cd: TestClientData, tester: ComponentTester[Props, State, _, TopNode]) {
    def observe(): Obs = {
      def inner = DomZipper(tester.component).down(">*", 2 of 2)
      new Obs(cd.project(), Try(new ReqTableObs(inner)))
    }
  }

  type Maybe[A] = Either[String, A]

  implicit def tryToEither[A](t: Try[A]): Maybe[A] =
    t match {
      case Success(s) => Right(s)
      case Failure(f) => Left(f.toString)
    }

  case class Obs(project: Project, reqTable: Maybe[ReqTableObs])

  @Lenses
  case class TestState(page: Page, project: Project)

  val * = Dsl.sync[Ref, Obs, TestState, String]

  def setPage(p: Page): *.Action =
    *.action(s"Set page to $p")
      .updateState(_.copy(page = p))
      .act(_.ref.tester.modProps(_.copy(page = p)))

  def applyEvents(e: Event*): *.Action =
    applyEvents(e.mkString("Apply events: ", ", ", "."), e: _*)

  def applyEvents(name: => String, e: Event*): *.Action =
    *.action(name)
      .updateStateO(s => o => s.copy(project = o.project))
      .act(_.ref.cd.applyTestEvents(e: _*))

  def testReqTable(action: RT.*.Action = Action.empty): *.Action =
    Test(action, RT.invariants)
      .pmapO[Obs](_.reqTable)
      .cmapS[TestState](TestState.project.get, (a, b) => TestState.project.set(b)(a)) // TODO Add Monocle support
      .cmapRef[Ref](_.tester.component zoomL State.reqTable)
      .asAction("Inspect ReqTable")

  val invariants: *.Check =
    Check.empty
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ProjectSpaTest extends TestSuite {
  PrepareEnv()

  import ProjectSpaDsl._

  def runTest(action: *.Action) = {
    val cp   = new TestClientProtocol
    val cd   = TestClientData(SampleProject3.project)
    val spa  = new ProjectSpaMain(MockRemotes.projectSPA, cp, cd)
    val rc   = MockRouterCtl[Page]()
    val init = TestState(Page.ReqTable, cd.project())

    ComponentTester(spa.Component)(Props(init.page, rc)) { tester =>
      val tt  = Test(action, invariants).observe(_.observe())
      val h   = tt.run(init, Ref(cd, tester))
      // println(h.format(History.Options.colored.alwaysShowChildren))
      h.assert(History.Options.colored)
      // println(h.format(History.Options.colored))
    }
  }

  def reqTableAfterLocalConfigUpdate: *.Action = (
    testReqTable(RT.showHideColumn("Priority") >> RT.sortBy("Priority"))
      >> setPage(Page.CfgFields)
      >> applyEvents("Delete Priority field", DeleteCustomField(priField, Delete))
      >> setPage(Page.ReqTable)
      >> testReqTable()
  )

  override def tests = TestSuite {
    'reqTableAfterLocalConfigUpdate - runTest(reqTableAfterLocalConfigUpdate)
  }
}
