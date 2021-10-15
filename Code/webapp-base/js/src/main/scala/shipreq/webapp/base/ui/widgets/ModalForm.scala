package shipreq.webapp.base.ui.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Element, raw}
import scala.scalajs.js
import scala.util.Success
import shipreq.base.util.{Disabled, Enabled, ErrorMsg}
import shipreq.webapp.base.ui.semantic.{JQuery, Modal, UsesSemanticUiManually}

object ModalForm {
  final case class SetState(form: Enabled, error: Option[ErrorMsg], inFlight: Boolean)
}

@UsesSemanticUiManually
abstract class ModalForm[A](name             : String,
                            initialResult    : A,
                            submitLabel      : String,
                            rootDom          : Element,
                            clear            : A => Boolean = (_: A) => true,
                            extraModalClasses: String = "",
                            ) {
  import ModalForm.SetState

  val id = Modal.nextId()

  private var onCompletion = Callback.empty
  private var lastResult = initialResult
  private var open = false

  protected def isModalOpen() = open

  protected final def getDom[N <: raw.Node](sel: String): CallbackTo[N] =
    CallbackTo(rootDom.querySelector(s"#$id $sel").domCast[N])

  def setState(s: SetState): Callback
  val clearFormData: Callback
  val header: TagMod
  val content: TagMod
  val justSubmit: AsyncCallback[SetState \/ A]

  protected final lazy val resetForm =
    clearFormData.when(clear(lastResult)) >> setState(SetState(Enabled, None, inFlight = false))

  protected lazy val onHide =
    Callback {open = false} >> resetForm >> Callback.suspend(onCompletion)

  private lazy val modalInitProps =
    js.Dynamic.literal(onHidden = onHide.toJsFn)

  private lazy val modalInit =
    Callback(JQuery.byId(id).modal(modalInitProps))

  private val modalShow =
    Callback {
      JQuery.byId(id).modal("show")
      open = true
    }

  protected val modalHide =
    Callback(JQuery(rootDom.querySelector("#" + id)).modal("hide"))

  protected def complete(a: A): Callback =
    Callback {lastResult = a} >> modalHide

  private val submitAsync: Option[ReactEvent] => AsyncCallback[Unit] = {
    val doIt = AsyncCallback.lazily(justSubmit).flatMap {
      case \/-(r) => complete(r).asAsyncCallback
      case -\/(s) => setState(s).asAsyncCallback
    }

    event =>
      for {
        _ <- event.map(_.preventDefaultCB).getOrEmpty.asAsyncCallback // prevent form submission
        _ <- setState(SetState(Disabled, None, inFlight = true)).asAsyncCallback
        _ <- doIt
      } yield ()
  }

  protected final val submit: Option[ReactEvent] => Callback =
    submitAsync(_).toCallback

  private val cancelButton =
    <.button(
      ^.cls := "ui button",
      ^.onClick --> modalHide,
      "Cancel")

  private lazy val submitButton =
    <.button(
      ^.cls := "ui button primary",
      ^.onClick ==> (e => submit(Some(e))),
      submitLabel)

  private def render: VdomElement =
    <.div(
      ^.id := id,
      ^.cls := s"ui $extraModalClasses modal",
      <.div(^.cls := "header", header),
      <.div(^.cls := "content", content),
      <.div(^.cls := "actions", cancelButton, submitButton),
    )

  lazy val component =
    ScalaComponent.builder.static(name)(render)
      .componentDidMountConst(modalInit)
      .build

  def run: AsyncCallback[A] = {
    val start: CallbackTo[AsyncCallback[A]] =
      for {
        (p, complete) <- AsyncCallback.promise[A]
        _             <- Callback {
                           lastResult = initialResult
                           onCompletion = CallbackTo(lastResult).flatMap(p => complete(Success(p)))
                         }
        _             <- resetForm
        _             <- modalShow
      } yield p

    for {
      promise <- start.asAsyncCallback
      result  <- promise
    } yield result
  }

}
