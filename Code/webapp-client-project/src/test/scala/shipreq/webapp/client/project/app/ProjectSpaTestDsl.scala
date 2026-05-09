package shipreq.webapp.client.project.app

import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import scala.util.{Failure, Success, Try}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.TestLocation
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.client.project.app.pages.admin.access.{AccessPageObs, AccessPageTestDsl}
import shipreq.webapp.client.project.app.pages.config.fields.{FieldConfigObs, FieldConfigTestDsl}
import shipreq.webapp.client.project.app.pages.config.issues.{IssueConfigObs, IssueConfigTestDsl}
import shipreq.webapp.client.project.app.pages.config.reqtypes.{ReqTypeConfigObs, ReqTypeConfigTestDsl}
import shipreq.webapp.client.project.app.pages.config.tags.{TagConfigObs, TagConfigTestDsl}
import shipreq.webapp.client.project.app.pages.content.issues.{IssuesPageObs, IssuesPageTestDsl => IP}
import shipreq.webapp.client.project.app.pages.content.reqdetail.{ReqDetailObs, ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.app.pages.content.reqgraph.{ReqGraphObs, ReqGraphTestDsl}
import shipreq.webapp.client.project.app.pages.content.reqtable.{ReqTableObs, ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.pages.root.LoadedRoot.Props
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.app.pages.root.{ProjectHomeTestDsl => PH, _}
import shipreq.webapp.client.project.test._
import shipreq.webapp.client.project.widgets.{ImplicationGraph, ReqSearch}
import shipreq.webapp.member.project.data.{ExternalPubid, Project}
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.member.protocol.entrypoint.ProjectSpaEntryPoint
import shipreq.webapp.member.test.WebappTestUtil.{PublicUserId1, Username1}
import shipreq.webapp.member.test._
import shipreq.webapp.member.test.project.SampleProject5
import shipreq.webapp.member.ui.OnlyVisibleOnMouseMove

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

  final case class Ref(global   : TestGlobal,
                       tester   : ComponentTester[Props, State, _],
                       confirmJs: TestConfirmJs,
                       promptJs : TestPromptJs,
                       ww       : TestWebWorkerClient,
                       loc      : TestLocation,
                      ) {

    def observe(): Obs = {
      val $ = tester.component.domZipper
      val inner = $(">div")(">div:nth-child(4)>*")
      val nav = new NavObs($(">nav"), inner)

      val base: Obs = {
        val e = Left("Chosen page is: " + nav.page)
        Obs(
          $,
          global.unsafeProject(),
          new TestGlobal.Obs($, global),
          new TestConfirmJs.Obs(confirmJs),
          nav,
          e, e, e, e, e, e, e, e, e, e)
      }

      nav.page match {
        case Page.Index        => base.copy(home        = Try(new ProjectHomeObs(inner)))
        case Page.CfgReqTypes  => base.copy(cfgReqTypes = Try(new ReqTypeConfigObs(inner, confirmJs)))
        case Page.CfgFields    => base.copy(cfgFields   = Try(new FieldConfigObs(inner)))
        case Page.CfgIssues    => base.copy(cfgIssues   = Try(new IssueConfigObs(inner)))
        case Page.CfgTags      => base.copy(cfgTags     = Try(new TagConfigObs(inner)))
        case Page.ReqTable     => base.copy(reqTable    = Try(new ReqTableObs(inner, base.global, base.confirmJs)))
        case Page.ReqDetail(_) => base.copy(reqDetail   = Try(new ReqDetailObs(inner, nav, base.global)))
        case Page.Issues       => base.copy(issues      = Try(new IssuesPageObs(inner)))
        case Page.ReqGraph     => base.copy(reqGraph    = Try(new ReqGraphObs(inner, base.global)))
        case Page.Access       => base.copy(access      = Try(new AccessPageObs(inner, base.global, base.confirmJs)))
      }
    }
  }

  final class NavObs(val nav: DomZipperJs, inner: DomZipperJs) {
    val breadcrumbs = nav(".ui.breadcrumb").collect0n(".section")
    // println(nav.innerHTML)

    val projectName: String =
      breadcrumbs.doms(1).textContent

    val dropdownCrumbName: Option[String] =
      nav.collect01(".ui.dropdown.inline").zippers.flatMap { z =>
        z.collect0n(">span,>.text,.header").zippers.iterator.map(_.domAsHtml.textContent.trim).find(_.nonEmpty)
      }

    val page: Page =
      dropdownCrumbName match {
        case Some("Req Table") => Page.ReqTable
        case Some("Content")   => Page.ReqDetail(ExternalPubid.parse(breadcrumbs.zippers.last.domAsHtml.textContent.trim).get)
        case Some("Req Graph") => Page.ReqGraph
        case Some("Fields")    => Page.CfgFields
        case Some("Req Types") => Page.CfgReqTypes
        case Some("Tags")      => Page.CfgTags
        case None              => Page.Index
        case Some("Issues")    => if (inner.exists(Style.issues.newIssueCont.selector)) Page.Issues else Page.CfgIssues
        case Some("Access")    => Page.Access
        case Some(n)           => sys error s"Unknown page: $n"
      }

    val unsavedChanges: Int =
      nav.collect01(".icon.edit").zippers.fold(0)(_.parent("span").domAsHtml.textContent.trim.toInt)
  }

  final case class Obs($          : DomZipperJs,
                       project    : Project,
                       global     : TestGlobal.Obs,
                       confirmJs  : TestConfirmJs.Obs,
                       nav        : NavObs,
                       home       : Maybe[ProjectHomeObs],
                       cfgFields  : Maybe[FieldConfigObs],
                       cfgIssues  : Maybe[IssueConfigObs],
                       cfgReqTypes: Maybe[ReqTypeConfigObs],
                       cfgTags    : Maybe[TagConfigObs],
                       issues     : Maybe[IssuesPageObs],
                       reqGraph   : Maybe[ReqGraphObs],
                       reqTable   : Maybe[ReqTableObs],
                       reqDetail  : Maybe[ReqDetailObs],
                       access     : Maybe[AccessPageObs],
                      ) {

    lazy val reqSearch: ReqSearchObs =
      new ReqSearchObs(nav.nav(Style.widgets.reqSearch.container.selector))
  }

  @Lenses
  final case class TestState(page: Page, project: Project, detailState: RD.State)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val * = Dsl[Ref, Obs, TestState]

  val global = new TestGlobal.TestDslWithObs(*)(_.global, _.global)

  val reqSearch = new ReqSearchObs.TestDsl(*)(_.reqSearch)

  val reauth = new TestReauthenticationModal.TestDsl(*)(_.global.reauthModal)

  implicit lazy val transformPH =
    PH.*.transformer
      .mapR[Ref](_.global)
      .pmapO[Obs](_.home)
      .mapS(TestState.project.get)((a, b) => TestState.project.replace(b)(a)) // TODO Add Monocle support

  implicit lazy val transformCRT =
    ReqTypeConfigTestDsl.*.transformer
      .mapR[Ref](_.confirmJs)
      .pmapO[Obs](_.cfgReqTypes)
      .mapS[TestState](_ => ())((s, _) => s)

  implicit lazy val transformRT =
    RT.*.transformer
      .mapR[Ref](r => RT.Ref(r.tester.component zoomStateL State.savedViews, r.global, r.promptJs, r.confirmJs))
      .pmapO[Obs](_.reqTable)
      .mapS(TestState.project.get)((a, b) => TestState.project.replace(b)(a)) // TODO Add Monocle support

  implicit lazy val transformRD =
    RD.*.transformer
      .mapR[Ref](_.global)
      .pmapO[Obs](_.reqDetail)
      .mapS[TestState](s => RD.TestState(s.project, s.detailState))((s, d) => TestState(s.page, d.project, d.state))

  implicit lazy val transformIP =
    IP.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.issues)
      .mapS[TestState](_ => ())((s, _) => s)

  implicit lazy val transformFieldConfig =
    FieldConfigTestDsl.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.cfgFields)
      .mapS[TestState](_ => ())((s, _) => s)

  implicit lazy val transformIssueConfig =
    IssueConfigTestDsl.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.cfgIssues)
      .mapS[TestState](_ => ())((s, _) => s)

  implicit lazy val transformTagConfig =
    TagConfigTestDsl.*.transformer
      .mapR[Ref](_ => ())
      .pmapO[Obs](_.cfgTags)
      .mapS[TestState](_ => ())((s, _) => s)

  implicit lazy val transformReqGraph =
    ReqGraphTestDsl.*.transformer
      .mapR[Ref](r => ReqGraphTestDsl.Ref(r.global, r.promptJs, r.ww))
      .pmapO[Obs](_.reqGraph)
      .mapS[TestState](_ => ())((s, _) => s)

  implicit lazy val transformAccessPage =
    AccessPageTestDsl.*.transformer
      .mapR[Ref](r => AccessPageTestDsl.Ref(r.global, r.confirmJs))
      .pmapO[Obs](_.access)
      .mapS[TestState](_ => ())((s, _) => s)

  private lazy val invariantsPH            = PH.invariants.lift
  private lazy val invariantsRT            = RT.invariants.lift
  private lazy val invariantsRD            = RD.invariants.lift
  private lazy val invariantsIP            = IP.invariants.lift
  private lazy val invariantsFieldConfig   = FieldConfigTestDsl.invariants.lift
  private lazy val invariantsReqTypeConfig = ReqTypeConfigTestDsl.invariants.lift
  private lazy val invariantsIssueConfig   = IssueConfigTestDsl.invariants.lift
  private lazy val invariantsTagConfig     = TagConfigTestDsl.invariants.lift
  private lazy val invariantsReqGraph      = ReqGraphTestDsl.invariants.lift
  private lazy val invariantsAccessPage    = AccessPageTestDsl.invariants.lift

  private val pageInvariants: *.Invariants =
    *.chooseInvariant("Page invariants")(_.state.page match {
      case Page.Index        => invariantsPH
      case Page.ReqTable     => invariantsRT
      case Page.ReqDetail(_) => invariantsRD
      case Page.Issues       => invariantsIP
      case Page.CfgFields    => invariantsFieldConfig
      case Page.CfgIssues    => invariantsIssueConfig
      case Page.CfgReqTypes  => invariantsReqTypeConfig
      case Page.CfgTags      => invariantsTagConfig
      case Page.ReqGraph     => invariantsReqGraph
      case Page.Access       => invariantsAccessPage
    })

  private val invariants: *.Invariants =
    pageInvariants.when(i => i.obs.nav.page ==* i.state.page) &
    *.focus("Page").obsAndState(_.nav.page, _.page).assert.equal &
    *.focus("Project name in NavBar").obsAndState(_.nav.projectName, _.project.name).assert.equal &
    global.fullscreenCount.test(_ + " should be ≤ 1")(_ <= 1)

  val unsavedChanges = *.focus("unsaved changes").value(_.obs.nav.unsavedChanges)

  val currentPage = *.focus("Current page").value(_.obs.nav.page)

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

  def liftProjectHomeTests    (p: PH                  .*.Plan): *.Plan = p.lift
  def liftReqTableTests       (p: RT                  .*.Plan): *.Plan = p.lift
  def liftReqDetailTests      (p: RD                  .*.Plan): *.Plan = p.lift
  def liftIssuePageTests      (p: IP                  .*.Plan): *.Plan = p.lift
  def liftReqTypeConfigTests  (p: ReqTypeConfigTestDsl.*.Plan): *.Plan = p.lift
  def liftFieldConfigPageTests(p: FieldConfigTestDsl  .*.Plan): *.Plan = p.lift
  def liftIssueConfigPageTests(p: IssueConfigTestDsl  .*.Plan): *.Plan = p.lift
  def liftTagConfigPageTests  (p: TagConfigTestDsl    .*.Plan): *.Plan = p.lift
  def liftReqGraphTests       (p: ReqGraphTestDsl     .*.Plan): *.Plan = p.lift
  def liftAccessPageTests     (p: AccessPageTestDsl   .*.Plan): *.Plan = p.lift

  def testReqTable(action: RT.*.Actions): *.Actions =
    liftReqTableTests(Plan.action(action)).asAction("Test ReqTable")

  def testReqDetail(action: RD.*.Actions): *.Actions =
    liftReqDetailTests(Plan.action(action)).asAction("Test ReqDetail")

  def runTest(action    : *.Actions,
              page      : Page,
              project   : Project                  = SampleProject5.project,
              rd        : RD.State                 = RD.unspecifiedState,
              wwPrep    : TestWebWorkerClient.Prep = TestWebWorkerClient.noInitialPrep,
              userId    : UserId.Public            = PublicUserId1,
              username  : Username                 = Username1,
              assertPass: Boolean                  = true,
             ): Unit = {
    runTestReturnReport(
      action     = action,
      page       = page,
      project    = project,
      rd         = rd,
      wwPrep     = wwPrep,
      userId     = userId,
      username   = username,
      assertPass = assertPass,
    )
    ()
  }

  def runTestReturnReport(action    : *.Actions,
                          page      : Page,
                          project   : Project                  = SampleProject5.project,
                          rd        : RD.State                 = RD.unspecifiedState,
                          wwPrep    : TestWebWorkerClient.Prep = TestWebWorkerClient.noInitialPrep,
                          userId    : UserId.Public            = PublicUserId1,
                          username  : Username                 = Username1,
                          assertPass: Boolean                  = true,
                         ): Report[String] = {

    ReqSearch.typingDelayMs = 0
    OnlyVisibleOnMouseMove.allowHide = false
    ImplicationGraph.runningInUnitTest = true

    val global       = TestGlobal(project, userId, username, ProjectCreator(userId))
    val confirmJs    = TestConfirmJs()
    val promptJs     = TestPromptJs()
    val projectId    = Obfuscated("pxx"): ProjectId.Public
    val creator      = ProjectCreator(userId)
    val initPageData = ProjectSpaEntryPoint.InitDataWithoutEncKey(username, userId, projectId, creator, project.name, AssetManifest(None), "/ww.js")
    val ww           = TestWebWorkerClient(wwPrep)
    val loc          = TestLocation()
    val ah           = AccessHandler.default(ww, loc)
    val spa          = new LoadedRoot(initPageData, global, confirmJs, promptJs, global.optionalFullscreen, ww, ah)
    val rc           = MockRouterCtl[Page]()
    val init         = TestState(page, global.unsafeProject(), rd)

    val report =
      try {
        ReactTestUtils.withRenderedIntoBody(spa.Component(Props(init.page, rc))) { m =>
          TestClipboard.clear()
          val tester = new ComponentTester(spa.Component)(m)
          Plan(action, invariants)
            .test(Observer(_.observe()))
            .withInitialState(init)
            .withRefByName(Ref(global, tester, confirmJs, promptJs, ww, loc))
            .run()
        }
      } finally {
        // Semantic UI adds modals outside of our React component
        TestUtil.removeSemanticUiFromBody()
      }

    if (assertPass)
      assertTestState(report)
      // assertTestState(r, println(s"${"=" * 120}\n${htmlScrub run tester.component.getDOMNode.map(_.asElement).outerHTML}\n"))

    report
  }
}
