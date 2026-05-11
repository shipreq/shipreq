package shipreq.webapp.member.feature.autocomplete

import japgolly.scalajs.react._
import japgolly.scalajs.react.util.JsUtil
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import scala.scalajs.js
import shipreq.webapp.base.util.{KeyHandler, KeyHandlers}
import shipreq.webapp.member.feature.autocomplete.Implicits._
import shipreq.webapp.member.feature.autocomplete.strategies.Strategies
import shipreq.webapp.member.jsfacade.TextComplete

object ForComponent {

  final case class Ctx[D](strategies: Strategies, dom: D) {
    override def equals(obj: Any): Boolean =
      obj match {
        case c: Ctx[_] =>
          ((dom, c.dom) match {
            case (a: AnyRef, b: AnyRef) => a eq b
            case (a, b)                 => a == b
          }) && (strategies ~=~ c.strategies)
        case _ => false
      }
  }
  object Ctx {
    implicit def univEq[D <: AnyRef]: UnivEq[Ctx[D]] = UnivEq.force
  }

  final case class AutoCompletable[D](newEditor: D => TextComplete.Editor) extends AnyVal

  /**
    * Public only for unit-tests. For React components, use one of the `install…` methods.
    */
  def lowLevelInstall[D](ctx: Ctx[D])(implicit ac: AutoCompletable[D]): CallbackTo[TextComplete] =
    CallbackTo {
      val editor = ac.newEditor(ctx.dom)
      val tc = new TextComplete(editor)
      tc.register(JsUtil.jsArrayFromTraversable(ctx.strategies))
      tc
    }

  trait Backend[D <: html.Element] {
    final type AutoCompleteCtx = Ctx[D]
    final def AutoCompleteCtx(strategies: Strategies, dom: D): AutoCompleteCtx = Ctx(strategies, dom)
    val autoCompleteCtx: CallbackOption[AutoCompleteCtx]

    private[this] var textComplete    : Option[TextComplete]    = None
    private[this] var textCompletePrev: Option[AutoCompleteCtx] = None
    private[this] var hideNext        : Boolean                 = false
    private[this] var dropdownVisible : Boolean                 = false

    final protected val textCompleteCBO: CallbackOption[TextComplete] =
      CallbackOption.option(textComplete)

    final def autoCompleteMount(implicit ac: AutoCompletable[D]): Callback =
      for {
        ctx <- autoCompleteCtx if ctx.strategies.nonEmpty
        tc  <- lowLevelInstall(ctx)(ac).toCBO
        _   <- addEventListeners(tc).toCBO
      } yield {
        textComplete = Some(tc)
        textCompletePrev = Some(ctx)
      }

    final def autoCompleteUpdate(implicit ac: AutoCompletable[D]): Callback =
      for {
        prev <- CallbackTo(textCompletePrev)
        next <- autoCompleteCtx.asCallback
        same  = prev ==* next
        _    <- (autoCompleteUnmount >> autoCompleteMount).unless(same)
        _    <- Callback.traverseOption(next)(_ => addEventListeners)
      } yield {
        textCompletePrev = next
        if (hideNext) {
          hideNext = false
          Callback {
            textComplete.foreach(_.hide())
          }.delayMs(1).toCallback.runNow()
        }
      }

    final def autoCompleteUnmount: Callback =
      Callback(
        textComplete.foreach(_.destroy())
      ).attempt >> Callback {
        textComplete = None
        textCompletePrev = None
      }

    final private val autoCompleteClose: Callback =
      Callback(textComplete.foreach(_.dropdown.deactivate())).attempt.void

    final protected def autoCompleteOnBlur =
      autoCompleteClose

    final protected val autoCompleteOnClick: ReactMouseEvent => Callback = e =>
      for {
        _ <- CallbackOption.unless(e.isDefaultPrevented())
        _ <- CallbackOption.require(e.target == e.currentTarget)
        _ <- e.preventDefaultCB.toCBO
        _ <- autoCompleteClose.toCBO
      } yield ()

    // Alternative to using autoCompleteOnKeyDown and autoCompleteOnKeyDownCapture
    final protected lazy val autoCompleteKeyHandlers: KeyHandlers =
      KeyHandlers(
        KeyHandler.Criterion.CtrlOrCmdSpace.handle(trigger) ::

        // asNonDefault is necessary here so that when tab is pressed when no dropdown is visible, it doesn't prevent
        // fallback behaviour (e.g. focusing the outer cell in table nav).
        KeyHandler.Criterion.TabCapture.handle(handleTab(_).asCallback.void).asNonDefault ::

        Nil)

    final protected val autoCompleteOnKeyDown: ReactKeyboardEvent => Callback = e =>
      CallbackOption.keyCodeSwitch(e, ctrlKey = true) {
        case KeyCode.Space => trigger
      }.asEventDefault(e)

    final protected val autoCompleteOnKeyDownCapture: ReactKeyboardEvent => Callback = e =>
      CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Tab => handleTab(e)
      }.asCallback.void

    final private val addEventListeners: Callback =
      for {
        tc <- textCompleteCBO
        _  <- addEventListeners(tc).toCBO
      } yield ()

    final private def addEventListeners(tc: TextComplete): Callback =
      Callback {
        if (tc._events.select.isEmpty) {
          tc.on("select", () => {
            hideNext = true
            dropdownVisible = false
          })
          tc.on("show", () => {
            dropdownVisible = true
          })
          tc.on("hide", () => {
            dropdownVisible = false
          })
        }
      }

    protected def getTextFromHeadToCaret: D => Option[String]

    /** Display the auto-complete options */
    final protected def trigger: CallbackOption[Unit] =
      trigger(getTextFromHeadToCaret)

    /** Display the auto-complete options */
    final protected def trigger(text: D => Option[String]): CallbackOption[Unit] =
      for {
        tc  <- textCompleteCBO
        ctx <- autoCompleteCtx
        txt <- CallbackOption.option(text(ctx.dom))
      } yield {
        tc.trigger(txt)
        ()
      }

    /** Focuses the next dropdown item */
    private def handleTab(e: ReactKeyboardEvent): CallbackOption[Unit] =
      for {
        tc <- textCompleteCBO
        _  <- CallbackOption.require(dropdownVisible)
        _  <- e.preventDefaultCB.toCBO
        _  <- e.stopPropagationCB.toCBO
        _  <- Callback(tc.editor.emitMoveEvent(js.Dynamic.literal(code = "DOWN"))).toCBO
      } yield ()
  }

  def install[P, C <: Children, S, B <: Backend[D], D <: html.Element: AutoCompletable]: ScalaComponent.Config[P, C, S, B, UpdateSnapshot.None, UpdateSnapshot.Some[Unit]] =
    _.componentDidMount(_.backend.autoCompleteMount)
      .componentDidUpdate(_.backend.autoCompleteUpdate)
      .componentWillUnmount(_.backend.autoCompleteUnmount)

}
