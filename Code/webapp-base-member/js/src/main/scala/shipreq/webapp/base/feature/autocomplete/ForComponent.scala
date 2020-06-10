package shipreq.webapp.base.feature.autocomplete

import japgolly.scalajs.react._
import japgolly.scalajs.react.internal.JsUtil
import japgolly.univeq._
import org.scalajs.dom.html
import shipreq.webapp.base.feature.autocomplete.Implicits._
import shipreq.webapp.base.feature.autocomplete.Utils.Strategies
import shipreq.webapp.base.jsfacade.TextComplete

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

    final protected val textCompleteCBO: CallbackOption[TextComplete] =
      CallbackOption.liftOption(textComplete)

    final def autoCompleteMount(implicit ac: AutoCompletable[D]): Callback =
      for {
        ctx <- autoCompleteCtx if ctx.strategies.nonEmpty
        tc  <- lowLevelInstall(ctx)(ac).toCBO
        _   <- addEventListeners(tc, ctx.dom).toCBO
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
        _    <- Callback.traverseOption(next)(ctx => addEventListeners(ctx.dom))
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

    final protected val autoCompleteBlur: Callback =
      Callback(textComplete.foreach(_.dropdown.deactivate())).attempt.void

    final private def addEventListeners(dom: D): Callback =
      for {
        tc <- textCompleteCBO
        _  <- addEventListeners(tc, dom).toCBO
      } yield ()

    final private def addEventListeners(tc: TextComplete, dom: D): Callback =
      Callback {
        if (tc._events.select.isEmpty) {
          tc.on("select", () => {
            hideNext = true
          })
        }
      }

    /** Display the auto-complete options */
    final protected def trigger(text: D => Option[String]): CallbackOption[Unit] =
      for {
        tc  <- textCompleteCBO
        ctx <- autoCompleteCtx
        txt <- CallbackOption.liftOption(text(ctx.dom))
      } yield {
        tc.trigger(txt)
        ()
      }
  }

  def install[P, C <: Children, S, B <: Backend[D], D <: html.Element: AutoCompletable]: ScalaComponent.Config[P, C, S, B, UpdateSnapshot.None, UpdateSnapshot.Some[Unit]] =
    _.componentDidMount(_.backend.autoCompleteMount)
      .componentDidUpdate(_.backend.autoCompleteUpdate)
      .componentWillUnmount(_.backend.autoCompleteUnmount)

}
