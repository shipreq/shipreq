package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import java.time.Instant
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter._
import shipreq.webapp.base.lib.ClientUtil
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui.semantic.{Icon, Menu, UsesSemanticUiManually}
import shipreq.webapp.client.project.app.Style.widgets.{reqSearch => *}
import shipreq.webapp.client.project.lib.DataReusability._

final class ReqSearch(SP: ReqSearch.StaticProps) {
  import ReqSearch._

  private val Component = ScalaComponent.builder[Props]
    .backend(new Backend(SP, _))
    .renderP(_.backend.render(_))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def render(p: Props): VdomElement =
    Component(p)

  def menuItem(p: Props): Menu.Item =
    Menu.Item(Menu.ItemType.Div(TagMod(*.menuItem, render(p))))
}

@UsesSemanticUiManually
object ReqSearch {

  final case class StaticProps(pxProject                     : Px[Project],
                               pxProjectConfig               : Px[ProjectConfig],
                               pxFilterCompilerFromFilterDead: Px[FilterDead => Filter.Valid.Compiler],
                               routerCtl                     : RouterCtl[ExternalPubid])

  final case class Props(state     : StateSnapshot[State],
                         filterDead: FilterDead,
                         pw        : ProjectWidgets.NoCtx)

  sealed trait FocusState
  object FocusState {
    case object Focused extends FocusState
    final case class LostFocus(at: Instant) extends FocusState
  }

  final case class State(text       : String,
                         focus      : Option[FocusState],
                         showResults: Boolean) {

    def hasQuery = text.trim.nonEmpty
  }

  object State {
    def init: State =
      State("", None, false)
  }

  // ===================================================================================================================

  private final val MaxResults = 10

  private final val TypingDelayMs = 200

  private final val BlurDelayMs = 300

  private val container =
    <.div(
      *.container,
      ^.cls := "ui icon input small")

  private val resultPopup =
    <.div(
      *.resultPopup,
      ^.cls := "ui popup bottom left visible fluid")

  private val resultsContainer =
    <.div(*.results)

  private val resultContainer =
    <.div(*.result)

  private val resultPubid =
    <.span(*.resultPubid)

  // ===================================================================================================================

  private sealed trait QueryResult

  private object QueryResult {
    case object InvalidFilter                     extends QueryResult
    case object NoFilter                          extends QueryResult
    final case class Results(reqs: ArraySeq[Req]) extends QueryResult
  }

  final class Backend(SP: StaticProps, $: BackendScope[Props, Unit]) {
    import SP._

    private val inputRef = Ref[html.Input]

    private val pxFilterDead =
      Px.props($).map(_.filterDead).withReuse.autoRefresh

    private val pxQueryText: Px[String] =
      Px.props($).map(_.state.value.text).withReuse.autoRefresh

    private val pxFilterValidator: Px[Filter.Validator] =
      pxProjectConfig.map(FilterAlgebra.validate)

    private val pxQueryResult: Px[QueryResult] =
      for {
        p    <- pxProject
        fv   <- pxFilterValidator
        ff   <- pxFilterCompilerFromFilterDead
        fd   <- pxFilterDead
        text <- pxQueryText
      } yield
        Filter.parseAndValidate(text, fv) match {

          case \/-(None) =>
            QueryResult.NoFilter

          case \/-(Some(validFilter)) =>
            val compiledFilter = ff(fd)(validFilter)
            // Unfortunately the filter is boolean rather than returning a Double score
            val reqs =
              MutableArray(compiledFilter.req.iterator(p.content.reqs.reqIterator()))
                .sortBy(_.pubid)(p.config.reqTypes.pubidOrdering)
                .iterator()
                .take(MaxResults)
                .to(ArraySeq)
            QueryResult.Results(reqs)

          case -\/(_) =>
            QueryResult.InvalidFilter
        }

    private lazy val updateShowResultsNow: Callback = {
      def update(s: State, now: Instant): Option[State] = {
        val showResults =
          s.hasQuery && (s.focus match {
            case None                           => false
            case Some(FocusState.Focused)       => true
            case Some(FocusState.LostFocus(at)) =>
              val sinceMs = now.toEpochMilli - at.toEpochMilli
              sinceMs < BlurDelayMs
          })
        Option.when(showResults != s.showResults)(s.copy(showResults = showResults))
      }

      lazy val repeatIfNecessary: Callback =
        $.props.flatMap { p =>
          val s = p.state.value
          val needRepeat =
            s.focus match {
              case Some(FocusState.LostFocus(_)) if s.showResults => true
              case _                                              => false
            }
          Callback.when(needRepeat)(updateShowResultsNow.delayMs(10).toCallback)
        }

      for {
        now <- ClientUtil.now
        p   <- $.props
        _   <- p.state.modStateOption(update(_, now), repeatIfNecessary)
      } yield ()
    }

    private val updateShowResultsOnTextChange: Callback =
      updateShowResultsNow.debounceMs(TypingDelayMs)

    private val onFocus: Callback =
      for {
        p <- $.props
        _ <- p.state.modState(s => s.copy(focus = Some(FocusState.Focused), showResults = s.hasQuery))
        _ <- inputRef.get.map(_.select()).toCallback
      } yield ()

    private val onBlur: Callback =
      for {
        now <- ClientUtil.now
        p   <- $.props
        _   <- p.state.modState(_.copy(focus = Some(FocusState.LostFocus(now))))
        _   <- updateShowResultsNow.delayMs(BlurDelayMs).toCallback
      } yield ()

    private def onTextChange(e: ReactEventFromInput): Callback = {
      val newText = e.target.value
      $.props.flatMap(_.state.modState(_.copy(text = newText), updateShowResultsOnTextChange))
    }

    private val inputBase =
      <.input.text(
        *.input,
        ^.placeholder := "Search...",
        ^.onFocus --> onFocus,
        ^.onBlur --> onBlur,
        ^.onChange ==> onTextChange,
      ).withRef(inputRef)

    private val onNav: Callback =
      $.props.flatMap(_.state.modState(_.copy(showResults = false, focus = None)))

    def render(p: Props): VdomNode = {
      val s = p.state.value
      val queryResult = pxQueryResult.value()

      var input = inputBase(^.value := s.text)

      queryResult match {
        case QueryResult.NoFilter | _: QueryResult.Results =>
        case QueryResult.InvalidFilter =>
          input = input(*.invalid)
      }

      val results = TagMod.when(s.showResults) {
        queryResult match {
          case QueryResult.NoFilter
             | QueryResult.InvalidFilter =>
            EmptyVdom

          case QueryResult.Results(reqs) =>
            resultPopup {
              if (reqs.isEmpty)
                "No results found."
              else
                resultsContainer {
                  val project = pxProject.value()
                  val routerCtl = SP.routerCtl.onSetRun(onNav)
                  reqs.toTagMod { req =>
                    resultContainer(
                      routerCtl.link(req.pubid.external(project))(
                        *.resultLink,
                        resultPubid(PlainText.pubidByReqId(req.id, project) + ":"),
                        p.pw.reqTitle(req),
                      )
                    )
                  }
                }
            }
        }
      }

      container(
        input,
        Icon.Search.tag,
        results)
    }
  }

  // ===================================================================================================================

  import Reusability.TemporalImplicitsWithoutTolerance.instant

  implicit val reusabilityF: Reusability[FocusState] = Reusability.derive
  implicit val reusabilityS: Reusability[State]      = Reusability.derive
  implicit val reusabilityP: Reusability[Props]      = Reusability.derive
  implicit val reusabilityC: Reusability[ReqSearch]  = Reusability.byRef
}
