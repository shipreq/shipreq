package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import monocle.Lens
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.{\/-, -\/}
import scalaz.syntax.equal._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.VerifiedEvents
import shipreq.webapp.base.filter.{FilterAst, FilterSpec}
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.state.{Changes, ChangeListener, ClientData}
import shipreq.webapp.client.app.Style.{reqtable => *}
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.data.FilterDead
import shipreq.webapp.client.feature._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.widgets.ProjectWidgets

object ReqTable extends StaticPropComponent.Template("ReqTable") {
  override protected def configureBackend = new Backend(_, _)
  override protected def configureRender  = _.renderBackend
  override protected def configure = _.configure(
    Listenable.install(_.static.cd, $ => ((c: Changes) => $.props.static.state_$.modState(_ recvChanges c))))

  case class StaticProps(cd             : ClientData,
                         cp             : ClientProtocol,
                         createContentFn: CreateContentFn.Instance,
                         updateContentFn: UpdateContentFn.Instance,
                         state_$        : CompState.Access[State])

  override type DynamicProps = State

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   filter      : FilterEditor.State,
                   selection   : RowSelection,
                   creation    : CreationInterface.State,
                   editStates  : EditState.Table,
                   asyncStates : AsyncState.TableState,
                   previewState: Preview.State,
                   modal       : Modal.State) {

    def recvChanges(changes: Changes): State =
      copy(project = changes.p2) // TODO This obviously affects other things
      // TODO A custom field removal/addition should affect ViewSettings

    def filterFailure(s: FilterEditor.State): State =
      copy(filter = s)

    def filterSuccess(fs: (FilterEditor.State, Option[FilterAst])): State = {
      val vs = viewSettings.copy(filter = fs._2)
      copy(viewSettings = vs, filter = fs._1)
    }

    def setFilterSpec(fs: FilterSpec): State = {
      val txt = FilterSpec toText fs
      FilterAst(project, fs) match {
        case \/-(ast) => filterSuccess(FilterEditor.State(txt, None), Some(ast))
        case -\/(err) => filterFailure(FilterEditor.State(txt, Some(err)))
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

    def init(cd: ClientData, fd: FilterDead, filterSpec: Option[FilterSpec]): State = {
      val proj = cd.project()
      var s = State(proj,
        ViewSettings      .default(fd),
        FilterEditor      .initialState,
        Selection         .empty,
        CreationInterface .initState,
        EditState         .empty,
        AsyncState        .initState,
        PreviewFeature    .initState,
        Modal             .none)
      filterSpec.foreach(f => s = s setFilterSpec f)
      s
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend(SP: StaticProps, $: BackendScope) extends OnUnmount {
    import SP._

    private def reusableStateFn[A](f: A => State => State): A ~=> Callback =
      ReusableFn(a => state_$.modState(f(a)))

    private def reusableSetState[A](l: Lens[State, A]): A ~=> Callback =
      reusableStateFn(l.set)

    private def reusableModState[A](l: Lens[State, A]): (A => A) ~=> Callback =
      reusableStateFn(l.modify)

    val setViewSettings = reusableSetState(State.viewSettings)
    val modViewSettings = reusableModState(State.viewSettings)
    val setSortCriteria = reusableSetState(State.sortCriteria)
    val setSelection    = reusableSetState(State.selection)
    val setModal        = reusableSetState(State.modal)
    val setCreation     = state_$ zoomL State.creation

    val project      = Px.bsMP($).propsM(_.project)
    val viewSettings = Px.bsMP($).propsM(_.viewSettings)
    val filterState  = Px.bsMP($).propsM(_.filter)
    val selection    = Px.bsMP($).propsM(_.selection)

    val vsVar      = viewSettings map (ReusableVar(_)(setViewSettings))
    val vsCols     = viewSettings map (_.columns)
    val colName    = project map Column.NameResolver.byProject reuse
    val plainText  = project map PlainText.apply
    val textSearch = Px.apply2(project, plainText)(TextSearch.apply)
    val widgets    = Px.apply2(project, plainText)(ProjectWidgets.apply)
    val colRnd     = Px.apply3(project, colName, widgets)(new ColumnRenderers(_, _, _))
    val colRnds    = Px.apply2(vsCols, colRnd)(_ map _.apply)
    val rows       = Px.apply4(viewSettings, project, plainText, textSearch)(Logic.rowsForTable).map(_.toVector)
    val stats      = Px.apply3(viewSettings, project, rows)(Logic.stats)

    val rowsWithAsyncWholeRowStatuses: Px.ThunkM[Set[Row.SourceId]] =
      Px.bsMP($).propsM(_.asyncStates.iterator
        .filter(_._2.rowStatus.isDefined)
        .map(_._1)
        .toSet)

    val visibleSelection =
      for {
        rs <- rows
        s  <- selection
        wr <- rowsWithAsyncWholeRowStatuses
      } yield
        s.updateBy(setSelection).legal(rs.iterator.map(_.sourceId).toSet &~ wr)

    val sortEditorProps =
      for {
        vs  <- viewSettings
        nr  <- colName
      } yield SortEditor.Props(vs.order, setSortCriteria, nr)

    private def callServer[I, F <: (I =>|=> VerifiedEvents)](remoteFn: RemoteFn.InstanceFor[F]): CallServer[I] =
      (i, sio, fio) =>
        cp.call(remoteFn)(
          i,
          s => cd.applyEventsS(s) >> sio,
          f => cp.consumeGenericFailure(f) >> fio(cp.genericFailureToText(f)))

    val createIO: CallServer[CreateContentCmd] =
      callServer(createContentFn)

    val updateIO: CallServer[UpdateContentCmd] =
      callServer(updateContentFn)

    val filterProps: FilterEditor.State => FilterEditor.Props = {
      import FilterEditor._
      val onFailure: OnFailure = ReusableFn(s => state_$.modState(_ filterFailure s))
      val onSuccess: OnSuccess = ReusableFn(i => state_$.modState(_.filterSuccess(i._1, i._2)))
      s => FilterEditor.Props(project.value(), onFailure, onSuccess, s)
    }

    val filterEditor: Px[ReusableVal[ReactElement]] =
      filterState map filterProps map ReusableVal.renderComponent(FilterEditor.Component)

    val asyncFeature = AsyncState.Feature(state_$)(State.asyncStates)

    val previewFeature = new PreviewFeature(state_$, State.previewState)

    val cellEditors: CellEditors =
      new CellEditorsImpl[State](state_$, State.editStates, asyncFeature, previewFeature, project, plainText, widgets,
        textSearch, updateIO)

    val creationInterface =
      new CreationInterface(setCreation, previewFeature, project, plainText, widgets, textSearch)

    // -----------------------------------------------------------------------------------------------------------------
    def render(s: DynamicProps): ReactElement = {
      Px.refresh(project, viewSettings, filterState, selection, rowsWithAsyncWholeRowStatuses)
      import Px.AutoValue._

      val cfg = s.project.config

      def vsProps = ViewSettingsEditor.Props(colName, cfg, vsVar, filterEditor)

      def creationProps = CreationInterface.Props(createIO, s.creation, s.previewState)

      def tableProps = Table.Props(
        project, rows, colName, colRnds, cellEditors, s.editStates,s.asyncStates, visibleSelection, modViewSettings)

      def selCtrlProps = SelectionCtrls.Props(
        visibleSelection, cfg, rows, setModal, project, widgets, plainText, textSearch, updateIO, asyncFeature)

      def mainScreen =
        <.div(
          ViewSettingsEditor.Component(vsProps),
          creationInterface.Component(creationProps),
          StatsSummary(stats),
          SelectionCtrls.Component(selCtrlProps),
          SortEditor.Component(sortEditorProps),
          Table.Component(tableProps))

      s.modal renderOrElse mainScreen
    }
  }

  // ===================================================================================================================

  val StatsSummary = ReactComponentB[TableStats]("Stats")
    .render_P(stats =>
      <.div(
        *.statsSummary,
        stats.summary))
    .configure(shouldComponentUpdate)
    .build
}
