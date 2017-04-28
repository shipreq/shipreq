package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._, vdom.html_<^._, MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.univeq._
import monocle.Lens
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.Direction
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.{PotentialFilter, ValidFilter}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.app.state.{Changes, ClientData}
import shipreq.webapp.client.project.app.Style.{reqtable => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.high.ProjectWidgets

object ReqTable {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[DynamicProps]("ReqTable")
      .backend(new Backend(staticProps, _))
      .renderBackend
      .build

  type InitEditor = ContentEditorFeature.D2.InitChild[Row, Column, FocusId]

  case class StaticProps(cd              : ClientData,
                         cp              : ClientProtocol,
                         createContentFn : CreateContentFn.Instance,
                         updateContentFn : UpdateContentFn.Instance,
                         pxPlainText     : Px[PlainText.ForProject],
                         pxTextSearch    : Px[TextSearch],
                         pxProjectWidgets: Px[ProjectWidgets],
                         initEditor      : InitEditor,
                         asyncFeature    : AsyncFeature.Feature.D2[Row.SourceId, Column, String],
                         asyncFeature2   : AsyncFeature.Feature.D2[Row.SourceId, Option[Column], String],
                         reqDetailRC     : RouterCtl[ExternalPubid],
                         state_$         : StateAccessPure[State])

  case class DynamicProps(editStates  : ContentEditorFeature.D2.State.ReadOnly[Row.SourceId, Column],
                          asyncStates : AsyncFeature.ReadOnly.D2[Row.SourceId, Option[Column], String],
                          preview     : PreviewFeature.Props.Composite[FocusId],
                          state       : State)

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   filter      : FilterEditor.State,
                   selection   : RowSelection,
                   creation    : CreationInterface.State,
                   modal       : Modal.State) {

    def updateProject(p2: Project): State = {
      val legal = Column.all(p2.config, viewSettings.filterDead).whole.toSet
      State(
        p2,
        viewSettings.filterColumns(legal.contains),
        filter,
        selection,
        creation,
        modal)
    }

    def filterFailure(s: FilterEditor.State): State =
      copy(filter = s)

    def filterSuccess(fs: (FilterEditor.State, Option[ValidFilter])): State = {
      val vs = viewSettings.copy(filter = fs._2)
      copy(viewSettings = vs, filter = fs._1)
    }

    def setFilterSpec(fs: PotentialFilter): State = {
      val txt = PotentialFilter toText fs
      PotentialFilter.validator(project)(fs) match {
        case \/-(f) => filterSuccess(FilterEditor.State(txt, None), Some(f))
        case -\/(e) => filterFailure(FilterEditor.State(txt, Some(e)))
      }
    }

    def setFilterDead(fd: FilterDead): State =
      if (viewSettings.filterDead ==* fd)
        this
      else
        State.viewSettings.modify(_ setFilterDead fd)(this)
  }

  object State {
    val sortCriteria = viewSettings ^|-> ViewSettings.order

    def init(cd: ClientData, fd: FilterDead, filterSpec: Option[PotentialFilter]): State = {
      val proj = cd.project()
      var s = State(proj,
        ViewSettings.default(fd),
        FilterEditor.initialState,
        Selection.empty,
        CreationInterface.initState,
        Modal.none)
      filterSpec.foreach(f => s = s setFilterSpec f)
      s
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend(val SP: StaticProps, $: BackendScope[DynamicProps, Unit]) extends OnUnmount {
    import SP._
    import cd.pxProject

    val setViewSettings = Reusable.fn.state(state_$ zoomStateL State.viewSettings).set
    val modViewSettings = Reusable.fn.state(state_$ zoomStateL State.viewSettings).mod
    val setSortCriteria = Reusable.fn.state(state_$ zoomStateL State.sortCriteria).set
    val setSelection    = Reusable.fn.state(state_$ zoomStateL State.selection).set
    val setModal        = Reusable.fn.state(state_$ zoomStateL State.modal).set
    val setCreation     = state_$ zoomStateL State.creation

    val pxViewSettings = Px.props($).map(_.state.viewSettings).withReuse.manualRefresh
    val pxFilterState  = Px.props($).map(_.state.filter)      .withReuse.manualRefresh
    val pxSelection    = Px.props($).map(_.state.selection)   .withReuse.manualRefresh

    val pxVsVar   = pxViewSettings map (StateSnapshot.withReuse(_)(setViewSettings))
    val pxVsCols  = pxViewSettings map (_.columns)
    val pxColName = pxProject map Column.NameResolver.byProject withReuse
    val pxColRnd  = Px.apply2(pxProject, pxProjectWidgets)(new ColumnRenderers(_, _))
    val pxColRnds = Px.apply2(pxVsCols, pxColRnd)(_ map _.apply)
    val pxRows    = Px.apply4(pxViewSettings, pxProject, pxPlainText, pxTextSearch)(Logic.rowsForTable).map(_.toVector)
    val pxStats   = Px.apply3(pxViewSettings, pxProject, pxRows)(Logic.stats)

    val pxRowsWithAsyncWholeRowStatuses: Px.ThunkM[Set[Row.SourceId]] =
      Px.props($).map(_.asyncStates.iterator
        .filter(_._2(None).isDefined)
        .map(_._1)
        .toSet
      ).withReuse.manualRefresh

    val pxVisibleSelection =
      for {
        rs <- pxRows
        s  <- pxSelection
        wr <- pxRowsWithAsyncWholeRowStatuses
      } yield
        s.updateBy(setSelection).legal(rs.iterator.map(_.sourceId).toSet &~ wr)

    val pxSortEditorProps =
      for {
        vs  <- pxViewSettings
        nr  <- pxColName
      } yield SortEditor.Props(vs.order, setSortCriteria, nr)

    val createIO: ServerCall[CreateContentCmd] =
      ServerCall.to(createContentFn, cp, cd)

    val updateIO: ServerCall[UpdateContentCmd] =
      ServerCall.to(updateContentFn, cp, cd)

    val filterProps: FilterEditor.State => FilterEditor.Props = {
      import FilterEditor._
      val onFailure: OnFailure = Reusable.fn(s => state_$.modState(_ filterFailure s))
      val onSuccess: OnSuccess = Reusable.fn(i => state_$.modState(_.filterSuccess(i._1, i._2)))
      s => FilterEditor.Props(pxProject.value(), onFailure, onSuccess, s)
    }

    val pxFilterEditor: Px[Reusable[VdomElement]] =
      pxFilterState.map { p0 =>
        val p = filterProps(p0)
        Reusable.implicitly(p).map(FilterEditor.Component(_))
      }

    val contentEditorFeature = {
      import ContentEditorFeature._

      val static = Static(
        initEditor.parent, initEditor.preview,
        pxProject, pxPlainText, pxProjectWidgets, pxTextSearch, updateIO)

      val newEditor: Row => Column => Option[NewEditor[FocusId]] = row => col => {
        @inline implicit def autoSome[P](e: NewEditor[P]): Option[NewEditor[P]] = Some(e)
        @inline def focusId = FocusId.AtCell(row.sourceId, col)

        def imps(row: ReqRow, dir: Direction) =
          Row.implications(dir).getOption(row).map(pubids =>
            NewEditor.ImplicationsAll(row.req, dir, pubids))

        row match {
          case r: ReqRow => col match {
            case Column.Code                                              => NewEditor.ReqCodesForReq(r.req)
            case Column.Title                                             => NewEditor.ReqTitle(r.req, focusId)
            case Column.Tags                                              => NewEditor.Tags(r.req, None)
            case Column.Implications(dir)                                 => imps(r, dir)
            case Column.CustomField(id: CustomField.Text       .Id, Live) => NewEditor.CustomTextField(r.req, id, focusId)
            case Column.CustomField(id: CustomField.Tag        .Id, Live) => NewEditor.Tags(r.req, Some(id))
            case Column.CustomField(id: CustomField.Implication.Id, Live) => NewEditor.ImplicationsCustomField(r.req, id)
            case Column.ReqType                                           => NewEditor.reqType(r.req)
            case Column.Pubid
               | Column.DeletionReason
               | Column.CustomField(_, Dead) => None
          }

          case r: ReqCodeGroupRow => col match {
            case Column.Code              => NewEditor.ReqCodeForReqCodeGroup(r.group)
            case Column.Title             => NewEditor.ReqCodeGroupTitle(r.group, focusId)
            case Column.Pubid
               | Column.ReqType
               | Column.Tags
               | Column.Implications(_)
               | Column.DeletionReason
               | Column.CustomField(_, _) => None
          }
        }
      }

//      initEditor.feature((row, col, lens) =>
//        D0.Feature(static)(newEditor(row)(col))(asyncFeature(row.sourceId)(col), lens, editability))
      ???
    }

    val creationInterface =
      new CreationInterface(setCreation, pxProject, pxPlainText, pxProjectWidgets, pxTextSearch)

    // -----------------------------------------------------------------------------------------------------------------
    def render(p: DynamicProps): VdomElement = {
      Px.refresh(pxViewSettings, pxFilterState, pxSelection, pxRowsWithAsyncWholeRowStatuses)
      import Px.AutoValue._
      import p.{state => s}

      val cfg = s.project.config

      def vsProps = ViewSettingsEditor.Props(pxColName, cfg, pxVsVar, pxFilterEditor)

      def creationProps = CreationInterface.Props(createIO, s.creation, p.preview)

      def tableProps = Table.Props(
        pxProject, pxRows, pxColName, pxColRnds, contentEditorFeature, p.editStates, p.asyncStates, pxVisibleSelection,
        modViewSettings)

      def selCtrlProps = SelectionCtrls.Props(
        pxVisibleSelection, cfg, pxRows, setModal, pxProject, pxProjectWidgets, pxPlainText, pxTextSearch, updateIO,
        asyncFeature2)

      def mainScreen =
        <.div(
          ViewSettingsEditor.Component(vsProps),
          creationInterface.Component(creationProps),
          StatsSummary(pxStats),
          SelectionCtrls.Component(selCtrlProps),
          SortEditor.Component(pxSortEditorProps),
          Table.Component(tableProps))

      s.modal renderOrElse mainScreen
    }
  }

  // ===================================================================================================================

  val StatsSummary = ScalaComponent.builder[TableStats]("Stats")
    .render_P(stats =>
      <.div(
        *.statsSummary,
        stats.summary))
    .configure(shouldComponentUpdate)
    .build
}
