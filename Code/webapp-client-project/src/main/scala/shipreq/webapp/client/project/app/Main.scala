package shipreq.webapp.client.project.app

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.{RouterCtl => RouterCtl_, _}
import monocle._
import monocle.macros._
import org.scalajs.dom
import scala.annotation.elidable
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalajs.js.annotation.JSExport
import shipreq.base.util.Intersection
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{ExternalPubid, FilterDead, HideDead, ReqId, ReqType, ReqTypePos}
import shipreq.webapp.base.protocol.{ClientFnDecl, ProjectSPA}
import shipreq.webapp.base.text.{PlainText, ProjectText, TextSearch}
import shipreq.webapp.client.base.protocol.{ClientFnImpl, ClientProtocol}
import shipreq.webapp.client.project.app.cfg.shared.Usage
import shipreq.webapp.client.project.app.state.{Changes, ClientData}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.high.{ImplicationGraph, ProjectWidgets}
import ContentEditorFeature.EditFieldKey

@JSExport(ClientFnDecl.ProjectSpaName)
object Main extends ClientFnImpl(ClientFnDecl.ProjectSpa) {

  override def run(remotes: ProjectSPA): Unit = {
    val cp = ClientProtocol.Default
    ClientData.init(cp, remotes.projectInit, cd => Callback {
      Style.addToDocument()
      val main    = new Main(remotes, cp, cd)
      val baseUrl = determineBaseUrl(dom.window.location.href)
      val router  = Router(baseUrl, main.routerConfig)
      router() render dom.document.getElementById("tgt")
    }).runNow()
  }

  // ===================================================================================================================
  // Routes

  type RouterCtl = RouterCtl_[Page]

  sealed trait Page
  object Page {
    case object Index       extends Page
    case object CfgFields   extends Page
    case object CfgIssues   extends Page
    case object CfgReqTypes extends Page
    case object CfgTags     extends Page
    case object ReqTable    extends Page
    case object ImpGraph    extends Page

    case class ReqDetail(pubid: ExternalPubid) extends Page {
      @elidable(elidable.INFO)
      override def toString = s"ReqDetail(${PlainText pubid pubid})"
    }

    object ReqDetail {
      val stringPrism: Prism[String, ReqDetail] =
        ExternalPubid.StringPrism ^<-> GenIso.fields[ReqDetail].reverse
    }
  }

  implicit def pageEq: UnivEq[Page] = UnivEq.derive

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  // ===================================================================================================================
  // UI

  val IndexComponent = ReactComponentB[RouterCtl]("Index")
    .render_P { ctl =>
      import Page._
      <.ul(
        Vector(ReqTable, ImpGraph, CfgFields, CfgIssues, CfgReqTypes, CfgTags).map(p =>
          <.li(ctl.link(p)(p.toString))))
    }
    .build

  // ===================================================================================================================
  // Component stuff (TODO Refactor)
  import reqtable.{Column, Row, ReqTable}
  import reqdetail.ReqDetail

  sealed trait FocusId
  object FocusId {
    case class Content(row: Row.SourceId, f: EditFieldKey) extends FocusId
    case class ReqTableCI(value: reqtable.FocusId.InCI) extends FocusId
    implicit def equality: UnivEq[FocusId] = UnivEq.derive

    val ToReqTable = Intersection[FocusId, reqtable.FocusId] {
      case Content(r, f) => Column.EditFieldKeyIntersection.reverse.getOptionMap(f, reqtable.FocusId.AtCell(r, _))
      case ReqTableCI(a) => Some(a)
    } {
      case reqtable.FocusId.AtCell(r, c) => Column.EditFieldKeyIntersection.getOptionMap(c, Content(r, _))
      case a: reqtable.FocusId.InCI      => Some(ReqTableCI(a))
    }
  }

  case class Props(page: Page, routerCtl: RouterCtl)

  sealed abstract class AsyncKey
  object AsyncKey {
    import shipreq.webapp.base.data.{CustomFieldId, UseCaseStepId, Live, Dead}
    import shipreq.webapp.client.project.app.reqdetail.Row.UseCaseSteps

    case object ReqType                              extends AsyncKey
    case object Code                                 extends AsyncKey
    case object Title                                extends AsyncKey
    case object Tags                                 extends AsyncKey
    case object ImplicationSrc                       extends AsyncKey
    case object ImplicationTgt                       extends AsyncKey
    case class CustomField       (id: CustomFieldId) extends AsyncKey
    case class UseCaseStep       (id: UseCaseStepId) extends AsyncKey
    case class UseCaseStepCtrls  (id: UseCaseStepId) extends AsyncKey
    case class AddUseCaseStep    (id: UseCaseStepId) extends AsyncKey
    case class AddUseCaseTailStep(s: UseCaseSteps)   extends AsyncKey

    @inline implicit def equality: UnivEq[AsyncKey] =
      UnivEq.derive

    implicit val reusability: Reusability[AsyncKey] =
      Reusability.byUnivEq

    import shipreq.webapp.client.project.app.reqtable.Column
    val ToReqTable = Intersection[AsyncKey, Column] {
      case ReqType               => Some(Column.ReqType              )
      case Code                  => Some(Column.Code                 )
      case Title                 => Some(Column.Title                )
      case Tags                  => Some(Column.Tags                 )
      case ImplicationSrc        => Some(Column.ImplicationSrc       )
      case ImplicationTgt        => Some(Column.ImplicationTgt       )
      case CustomField(id)       => Some(Column.CustomField(id, Live))
      case UseCaseStep       (_)
         | UseCaseStepCtrls  (_)
         | AddUseCaseStep    (_)
         | AddUseCaseTailStep(_) => None
    } {
      case Column.ReqType               => Some(ReqType        )
      case Column.Code                  => Some(Code           )
      case Column.Title                 => Some(Title          )
      case Column.Tags                  => Some(Tags           )
      case Column.ImplicationSrc        => Some(ImplicationSrc )
      case Column.ImplicationTgt        => Some(ImplicationTgt )
      case Column.CustomField(id, Live) => Some(CustomField(id))
      case Column.Pubid
           | Column.DeletionReason
           | Column.CustomField(_, Dead)  => None
    }

    import shipreq.webapp.client.project.app.reqdetail.Cell
    val ToReqDetail = Intersection[AsyncKey, Cell] {
      case ReqType               => Some(Cell.ReqType              )
      case Code                  => Some(Cell.Code                 )
      case Title                 => Some(Cell.Title                )
      case Tags                  => Some(Cell.Tags                 )
      case ImplicationSrc        => Some(Cell.ImplicationSrc       )
      case ImplicationTgt        => Some(Cell.ImplicationTgt       )
      case CustomField(id)       => Some(Cell.CustomField(id)      )
      case UseCaseStep(id)       => Some(Cell.UseCaseStep(id)      )
      case UseCaseStepCtrls(id)  => Some(Cell.UseCaseStepCtrls(id) )
      case AddUseCaseStep(id)    => Some(Cell.AddUseCaseStep(id)   )
      case AddUseCaseTailStep(s) => Some(Cell.AddUseCaseTailStep(s))
    } {
      case Cell.ReqType               => Some(ReqType              )
      case Cell.Code                  => Some(Code                 )
      case Cell.Title                 => Some(Title                )
      case Cell.Tags                  => Some(Tags                 )
      case Cell.ImplicationSrc        => Some(ImplicationSrc       )
      case Cell.ImplicationTgt        => Some(ImplicationTgt       )
      case Cell.CustomField(id)       => Some(CustomField(id)      )
      case Cell.UseCaseStep(id)       => Some(UseCaseStep(id)      )
      case Cell.UseCaseStepCtrls(id)  => Some(UseCaseStepCtrls(id) )
      case Cell.AddUseCaseStep(id)    => Some(AddUseCaseStep(id)   )
      case Cell.AddUseCaseTailStep(s) => Some(AddUseCaseTailStep(s))
    }
  }

  @Lenses
  case class State(editStates  : ContentEditorFeature.D2.State.Simple[Row.SourceId, EditFieldKey],
                   asyncStates : AsyncActionFeature.D2.State.Simple[Row.SourceId, AsyncKey, String],
                   previewState: PreviewFeature.State[FocusId],
                   filterDead  : FilterDead,
                   reqTable    : ReqTable.State,
                   reqDetail   : ReqDetail.State)
}


// =====================================================================================================================
final class Main(r: ProjectSPA, cp: ClientProtocol, cd: ClientData) {
  import Main._

  def routerConfig =
    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._
      import Page._

      val reqTablePath = "/table"

      val dynPage = dynRenderR((page: Page, r) => Component(Props(page, r)))

      def staticPage(route: StaticDsl.Route[Unit], page: Page) =
        staticRoute(route, page) ~> renderR(r => Component(Props(page, r)))

      def reqDetailRoute =
        dynamicRouteCT(reqTablePath / remainingPath.pmapL(ReqDetail.stringPrism)) ~> dynPage autoCorrect

      ( staticPage(root           , Index      )
      | staticPage(reqTablePath   , ReqTable   )
      | staticPage("/imps"        , ImpGraph   )
      | staticPage("/cfg/fields"  , CfgFields  )
      | staticPage("/cfg/issues"  , CfgIssues  )
      | staticPage("/cfg/reqtypes", CfgReqTypes)
      | staticPage("/cfg/tags"    , CfgTags    )
      | reqDetailRoute
      | trimSlashes
      ).notFound(redirectToPage(Index)(Redirect.Replace))
        .verify(
          Index, ReqTable, CfgFields, CfgIssues, CfgReqTypes, CfgTags,
          ReqDetail(ExternalPubid(ReqType.Mnemonic("A"), ReqTypePos(1))))
    }

  // ===================================================================================================================
  import reqtable.{Column, Row, ReqTable, ViewSettings}
  import reqdetail.{ReqDetail, Cell}

  val reqTableVS = State.reqTable ^|-> ReqTable.State.viewSettings

  def initState = State(
    ContentEditorFeature.D2.State.init,
    AsyncActionFeature.D2.State.init,
    PreviewFeature.initState,
    HideDead,
    ReqTable.State.init(cd, HideDead, None),
    ReqDetail.initState)

  // ===================================================================================================================
  class Backend($: BackendScope[Props, State]) extends OnUnmount {
    import cd.pxProject

    // This never changes
    val routerCtl = $.props.runNow().routerCtl
    val reqDetailRC = routerCtl.contramap(Page.ReqDetail.apply)

    val setFilterDead: FilterDead ~=> Callback =
      ReusableFn(fd => $.modState(
        State.filterDead.set(fd) compose
        reqTableVS.modify(_ setFilterDead fd)))

    val pxPlainText      = pxProject.map(PlainText(_, ProjectText.Context.None))
    val pxTextSearch     = Px.apply2(pxProject, pxPlainText)(TextSearch.apply)
    val pxProjectWidgets = Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))

    val asyncFeature: AsyncActionFeature.D2.Feature[Row.SourceId, AsyncKey, String] =
      AsyncActionFeature.D2.Feature($ zoomL State.asyncStates)

    val previewFeature = new PreviewFeature($, State.previewState)

    def initReqTableEditor: ReqTable.InitEditor = {
      import ContentEditorFeature._
      new D2.InitChild[Row, Column, reqtable.FocusId] {
        override type Parent    = State
        override val parent     = $: CompState.Access[Parent]
        override val preview    = previewFeature.mapK(FocusId.ToReqTable)
        override val editorLens =
          (r: Row, c: Column) =>
            Column.EditFieldKeyIntersection.getOption(c).map(efk =>
              State.editStates ^|-> D2.State.at(r.sourceId) ^|-> D1.State.at(efk))
      }
    }

    val reqTable = ReqTable(ReqTable.StaticProps(
      cd, cp, r.createContent, r.updateContent,
      pxPlainText, pxTextSearch, pxProjectWidgets,
      initReqTableEditor,
      asyncFeature.mapK1(AsyncKey.ToReqTable),
      reqDetailRC,
      $ zoomL State.reqTable))

    val pxReqDetailId = Px(None: Option[ReqId])

    val pxReqDetailReqProps: Px[Option[State => ReqDetail.ReqProps]] =
      pxReqDetailId.map(_.map { id =>
        val r = Row.ReqRowSourceId(id)

        val focusIdToCell = Intersection[FocusId, Cell] {
          case FocusId.Content(rs, f) =>
            if (r ==* rs)
              Cell.EditFieldKeyIntersection.reverse.getOption(f)
            else
              None
          case FocusId.ReqTableCI(_) => None
        }(c => Cell.EditFieldKeyIntersection.getOptionMap(c, FocusId.Content(r, _)))

        import ContentEditorFeature._
        val initEditor =
          new D1.InitChild[Cell, Cell] {
            override type Parent    = State
            override val parent     = $: CompState.Access[Parent]
            override val preview    = previewFeature.mapK(focusIdToCell)
            override val editorLens =
              (c: Cell) =>
                Cell.EditFieldKeyIntersection.getOption(c).map(efk =>
                  State.editStates ^|-> D2.State.at(r) ^|-> D1.State.at(efk))
          }

        val asyncF1 = asyncFeature(r).mapK(AsyncKey.ToReqDetail)

        (s: State) =>
          ReqDetail.ReqProps(
            initEditor,
            asyncF1,
            s.editStates(r).mapK(Cell.EditFieldKeyIntersection.reverse),
            s.asyncStates(r).mapK(AsyncKey.ToReqDetail))
      })

    def reqDetailReqPropsFn(s: State) = (id: ReqId) => {
      pxReqDetailId.set(Some(id))
      pxReqDetailReqProps.value().get(s)
    }

    def ww = WebWorkerClient.Instance

    val reqDetail = ReqDetail(ReqDetail.StaticProps(
      cd, cp, reqDetailRC, ww, r.updateContent,
      pxPlainText, pxTextSearch, pxProjectWidgets))

    val reqDetailSetState: ReqDetail.State ~=> Callback =
      ReusableFn($.zoomL(State.reqDetail).setState(_))

    val usageShow =
      Usage.Show((fd, fs) =>
        routerCtl
          .onSet($.modState(State.reqTable.modify(_.setFilterDead(fd).setFilterSpec(fs()))) >> _)
          .link(Page.ReqTable))

    def render(p: Props, s: State): ReactElement = {
      def fd = ReusableVar(s.filterDead)(setFilterDead)

      def layout(content: ReactElement, backPage: Page = Page.Index) =
        <.div(
          <.div(
            ^.textAlign.right,
            ^.paddingRight := "0.6ex",
            ^.marginTop := "-14px",
            routerCtl.link(backPage)("← Back")),
          content)

      p.page match {

        case Page.Index =>
          IndexComponent(routerCtl)

        case Page.CfgFields =>
          layout(cfg.fields.CfgFields.Props(cp, r.fieldCrud, cd, fd).component)

        case Page.CfgIssues =>
          layout(cfg.issues.CfgIssues.Props(cp, r.issueTypeCrud, r.reqTypeImpMod, r.fieldMandMod, cd, fd, usageShow).component)

        case Page.CfgReqTypes =>
          layout(cfg.reqtypes.CfgReqTypes.Props(cp, r.reqTypeCrud, cd, fd, usageShow).component)

        case Page.CfgTags =>
          layout(cfg.tags.CfgTags.Props(cp, r.tagCrud, cd, fd).component)

        case Page.ReqTable =>
          layout(reqTable(ReqTable.DynamicProps(
            s.editStates.mapK1(Column.EditFieldKeyIntersection.reverse),
            s.asyncStates.mapK1(AsyncKey.ToReqTable),
            s.previewState.mapK(FocusId.ToReqTable),
            s.reqTable)))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            pubid,
            fd,
            reqDetailReqPropsFn(s),
            ReusableVar(s.reqDetail)(reqDetailSetState))
          layout(reqDetail(props), Page.ReqTable)

        case Page.ImpGraph =>
          val p = cd.project()
          val ig = ImplicationGraph.Props(
            None, s.filterDead,
            p.implications, p.reqs, p.config.reqTypes,
            pxPlainText.value(),
            reqDetailRC,
            ww)
          layout(ig.render)
      }
    }

    def onProjectChange(c: Changes): Callback =
      $.modState(State.reqTable.modify(_ updateProject c.p2))
  }

  val Component = ReactComponentB[Props]("")
    .initialState(initState)
    .renderBackend[Backend]
    .configure(Listenable.install(_ => cd, _.backend.onProjectChange))
    .build
}
