package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.window
import shipreq.webapp.base.protocol.{AjaxClient, CommonProtocols}
import shipreq.webapp.base.protocol.CommonProtocols.ReportClientError.ErrorInfo

/** Wrap top-level components in this in order to catch React errors.
 *
 * Usage:
 *
 *     1. Wrap your top-level component in this
 *     2. Call `ErrorHandlingFeature.enable()`
 *     3. Create a [[ErrorHandlingFeature.StateRecorder]] for your state (and use it obviously)
 */
object ErrorHandlingFeature {

  /** This is required so that we can assume singleton SPAs.
    * In tests, we load the same SPA multiple times and don't want previous state being restored.
    */
  def enable(): Unit = {
    StateRecorder.enabled = true
  }

  // ===================================================================================================================

  // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  import scala.scalajs.js
  /** The data captured by React when it catches an error for the `componentDidCatch` event.
   *
   * @param rawError The JS error. Usually a [[js.Error]].
   *                 If you threw an error using `throw js.JavaScriptException("OMG")` then is value is just
   *                 the argument `"OMG"`.
   * @since 1.6.0
   */
  final case class ReactCaughtError(rawError: js.Any, rawInfo: raw.React.ErrorInfo) {
    import scala.scalajs.runtime.StackTrace

    override def toString: String =
      s"ReactCaughtError($rawErrorString)"

    def rawErrorString: String =
      try "" + rawError catch {case _: Throwable => ""}

    @inline def componentStack: String =
      rawInfo.componentStack

    def dynError: js.Dynamic =
      rawError.asInstanceOf[js.Dynamic]

    val typeOfError: String =
      js.typeOf(rawError)

    def stackTraceElements: Array[StackTraceElement] =
      StackTrace.extract(dynError)

    def stack: String =
      stackTraceElements.iterator.map(_.toString).filter(_.nonEmpty).mkString("\n")

    val jsError: Either[Any, js.Error] =
      rawError match {
        case e: js.Error => Right(e)
        case a           => Left(a)
      }

    def name: String =
      jsError.fold(_ => typeOfError, _.name)

    def message: String =
      jsError.fold(_ => rawErrorString, _.message)
  }
  // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private final case class Props(view  : VdomElement,
                                 submit: ErrorInfo => Callback) {
    @inline def render: VdomElement = Component(this)
  }

  private final case class State(errors: Int)

  private final class Backend($: BackendScope[Props, State]) {

    def render(p: Props): VdomElement =
      p.view

    private val alertUserThatAnErrorOccurred: Callback =
      Callback {
        window.alert(
          s"""
             |An error we've never seen before has occurred.
             |
             |A report has been captured for our development team to fix this as soon as possible. If you wish to follow up on this, feel free to talk to us via the send-feedback link in the top-right under your username.
             |
             |We apologise for the inconvenience.
             |""".stripMargin.trim)
      }

    def onError(e: ReactCaughtError): Callback = {

      def errorInfo(s: State): ErrorInfo =
        ErrorInfo(
          name           = e.name,
          message        = e.message,
          stack          = e.stack,
          componentStack = e.componentStack,
          other          = Map(
            "error"       -> e.rawErrorString,
            "typeOfError" -> e.typeOfError,
          ))

      for {
        p <- $.props
        s <- $.state
        _ <- p.submit(errorInfo(s)).attempt
        _ <- alertUserThatAnErrorOccurred
        _ <- $.modState(s => State(s.errors + 1)) // refreshes screen
      } yield ()
    }
  }

  private val Component = ScalaComponent.builder[Props]("ErrorHandling")
    .initialState(State(0))
    .renderBackend[Backend]
    .componentDidCatch($ => $.backend.onError(ReactCaughtError($.error, $.info)))
    .build

  def apply(view      : VdomElement,
            metadata  : CallbackTo[CommonProtocols.Metadata.Client],
            ajaxClient: AjaxClient.Binary = AjaxClient.BinaryImpl): VdomElement = {

    val ssp = ajaxClient.invoker(CommonProtocols.ReportClientError.ajax)

    val submit: ErrorInfo => Callback =
      e => metadata
        .map(CommonProtocols.ReportClientError.Request(e, _))
        .flatMap(ssp(_).toCallback)

    Props(view, submit).render
  }

  // ===================================================================================================================

  /** Because stupid React unmounts components on error, even if they're wrapped in an error boundary like the one above,
    * re-rendering it means the state is lost.
    *
    * Luckily ShipReq is smart in that there's always a single component with all the SPA's state in it which makes this
    * hacky little workaround feasible.
    *
    * Usage:
    *
    *     1. Create an instance of this.
    *     2. Initialise the stateful root component with `yourStateRecorder.getOrElse(State.init)`
    *     3. Call `yourStateRecorder.record(state)` in said component's render function.
    */
  final class StateRecorder[S >: Null] {
    private var last: S = null
    private var secondLast: S = null

    def record(s: S): Unit = {
      secondLast = last
      last = s
    }

    def getOrElse(s: => S): S =
      if (StateRecorder.enabled && secondLast != null)
        secondLast
      else
        s
  }

  object StateRecorder {
    private[ErrorHandlingFeature] var enabled = false

    def apply[S >: Null]: StateRecorder[S] =
      new StateRecorder
  }
}
