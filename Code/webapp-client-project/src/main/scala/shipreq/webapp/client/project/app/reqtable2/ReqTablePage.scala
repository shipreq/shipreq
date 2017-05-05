package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.univeq._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.{PotentialFilter, ValidFilter}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.project.app.state.{Changes, ClientData}
import shipreq.webapp.client.project.app.Style.{reqtable2 => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.ProjectWidgets

object ReqTablePage {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[Props]("ReqTablePage")
      .backend(new Backend(staticProps, _))
      .renderBackend
      .build

  final case class StaticProps(stateAccess     : StateAccessPure[State],
                               cd              : ClientData,
//                               cp              : ClientProtocol,
//                               createContentFn : CreateContentFn.Instance,
//                               updateContentFn : UpdateContentFn.Instance,
                               pxPlainText     : Px[PlainText.ForProject],
                               pxTextSearch    : Px[TextSearch],
                               pxProjectWidgets: Px[ProjectWidgets],
                               reqDetailRC     : RouterCtl[ExternalPubid])

  final case class Props(editor    : EditorFeature.ReadWrite.ForProject,
                         rowAsync  : AsyncFeature.ReadWrite.D1[Row.SourceId, String],
                         filterDead: FilterDead,
                         state     : State)

  object Props {
    implicit val reusability: Reusability[Props] =
      Reusability.caseClass
  }

  @Lenses
  final case class State(tableSettings: TableSettings,
                         selection    : RowSelection,
                         modal        : Modal.State)

  object State {
    implicit val reusability: Reusability[State] =
      Reusability.caseClass

    def init: State =
      State(
        TableSettings.default,
        Selection.empty,
        Modal.none)
  }

  final class Backend(sp: StaticProps, $: BackendScope[Props, Unit]) {
    import sp._
    import cd.pxProject

    val modSettings : ModFn[TableSettings] = Reusable.fn.state(stateAccess zoomStateL State.tableSettings).mod
    val setSelection: SetFn[RowSelection ] = Reusable.fn.state(stateAccess zoomStateL State.selection).set

    private var manualRefresh = List.empty[Px.ThunkM[_]]
    private def pxProps[A: Reusability](f: Props => A): Px.ThunkM[A] = {
      val px = Px.props($).map(f).withReuse.manualRefresh
      manualRefresh ::= px
      px
    }

    val pxFilterDead   : Px[FilterDead            ] = pxProps(_.filterDead)
    val pxTableSettings: Px[TableSettings         ] = pxProps(_.state.tableSettings)
    val pxSelection    : Px[RowSelection          ] = pxProps(_.state.selection)
    val pxColumns      : Px[NonEmptyVector[Column]] = pxProps(_.state.tableSettings.columns)

    val pxRows: Px[Vector[Row]] =
      for {
        p  <- pxProject
        s  <- pxTableSettings
        fd <- pxFilterDead
        pt <- pxPlainText
        ts <- pxTextSearch
      } yield Logic.rowsForTable(p, s, fd, pt, ts).toVector

    val pxRowIdsWithWholeRowAsync: Px[Set[Row.SourceId]] =
      pxProps(_.rowAsync.read.keySet)

    val pxRowSelectionVisible: Px[RowSelectionVisible] =
      for {
        rs <- pxRows
        wr <- pxRowIdsWithWholeRowAsync
        s  <- pxSelection
      } yield
        s.updateBy(setSelection).legal(rs.iterator.map(_.sourceId).toSet &~ wr)

    val pxColumnsPlus: Px[NonEmptyVector[ColumnPlus]] =
      (for {
        p  <- pxProject
        cs <- pxColumns
      } yield ColumnPlus.forceNEV(ColumnPlus.byProject(p))(cs))
        .withReuse

    def render(p: Props): VdomElement = {
      Px.refresh(manualRefresh: _*)

      val table = Table.Whole.Props(
        pxRows.value(),
        pxColumnsPlus.value(),
        pxRowSelectionVisible.value(),
        p.editor,
        p.rowAsync.read,
        pxProject.value().config.reqTypes,
        pxProjectWidgets.value(),
        modSettings,
      ).render

      <.main(BaseStyles.containerFull,
        table)
    }
  }
}
