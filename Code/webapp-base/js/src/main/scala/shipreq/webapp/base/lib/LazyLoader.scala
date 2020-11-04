package shipreq.webapp.base.lib

import japgolly.scalajs.react.{Callback, CallbackTo}
import org.scalajs.dom.{document, html}
import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.Dynamic
import scalaz.Applicative
import scalaz.std.list.listInstance
import scalaz.syntax.traverse._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.lib.LazyLoader.State

final class LazyLoader[A](initState: State.Pending[A]) {

  private var _state: State[A] =
    initState

  private val state: CallbackTo[State[A]] =
    CallbackTo(_state)

  private def modState(f: State[A] => State[A]): CallbackTo[State[A]] =
    CallbackTo {
      _state = f(_state)
      _state
    }

  private def setState(s: State[A]): Callback =
    modState(_ => s).void

  def onLoad(onLoad: A => Callback, runIfAlreadyLoaded: Boolean = true): Callback =
    state flatMap {
      case State.Loaded(a)          => if (runIfAlreadyLoaded) onLoad(a) else Callback.empty
      case State.Loading(cbs)       => setState(State.Loading(onLoad :: cbs))
      case State.Pending(load, cbs) => setState(State.Pending(load, onLoad :: cbs))
    }

  def load(onLoad: A => Callback, runIfAlreadyLoaded: Boolean = true): Callback =
    this.onLoad(onLoad, runIfAlreadyLoaded) >> load

  def load: Callback =
    state flatMap {
      case State.Pending(f, cbs)              => setState(State.Loading(cbs)) >> f(setValue)
      case State.Loaded(_) | State.Loading(_) => Callback.empty
    }

  private def setValue(a: A): Callback =
    for {
      oldState <- state
      _ <- setState(State.Loaded(a))
      cbs: List[A => Callback] = oldState match {
        case State.Loading(callbacks)    => callbacks
        case State.Pending(_, callbacks) => callbacks
        case State.Loaded(_)             => Nil
      }
      _ <- Callback.traverse(cbs)(_ apply a)
    } yield ()

  def currentValue(): Option[A] =
    _state match {
      case State.Loaded(a)     => Some(a)
      case State.Loading(_)
         | State.Pending(_, _) => None
    }

  def use[B](now: Option[A] => B, onLoad: A => Callback = _ => Callback.empty): B = {
    val v = currentValue()
    if (v.isEmpty)
      load(onLoad).runNow()
    now(v)
  }

  def map[B](f: A => B): LazyLoader[B] =
    LazyLoader.async[B](g => load(g compose f))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object LazyLoader {

  def sync[A](a: => A): LazyLoader[A] =
    async(f => f(a))

  def async[A](load: (A => Callback) => Callback): LazyLoader[A] =
    new LazyLoader(State.Pending(load, Nil))

  def css(cdn: AssetManifest.CDN): LazyLoader[Unit] =
    css(cdn.href, cdn.integrity)

  def css(href: String, integrity: Option[String]): LazyLoader[Unit] =
    LazyLoader.async(f => Callback {
      val crossorigin = Option.when(href contains "://")("anonymous")
      val e = document.createElement("link").asInstanceOf[html.Link]
      e.href = href
      e.rel = "stylesheet"
      integrity.foreach(e.asInstanceOf[Dynamic].integrity = _)
      crossorigin.foreach(e.asInstanceOf[Dynamic].crossOrigin = _)
      e.asInstanceOf[html.Script].onload = f(()).toJsFn1
      document.head.appendChild(e)
    })

  def js(cdn: AssetManifest.CDN): LazyLoader[Unit] =
    js(cdn.href, cdn.integrity)

  def js(href: String, integrity: Option[String]): LazyLoader[Unit] =
    LazyLoader.async(f => Callback {
      val crossorigin = Option.when(href contains "://")("anonymous")
      val e = document.createElement("script").asInstanceOf[html.Script]
      e.src = href
      e.async = true
      integrity.foreach(e.asInstanceOf[Dynamic].integrity = _)
      crossorigin.foreach(e.asInstanceOf[Dynamic].crossOrigin = _)
      e.onload = f(()).toJsFn1
      document.head.appendChild(e)
    })

  def merge(lls: LazyLoader[_]*): LazyLoader[Unit] =
    if (lls.isEmpty)
      sync(())
    else
      lls.toList.map(_.void).sequence_

  sealed trait State[A]
  object State {
    final case class Pending[A](load: (A => Callback) => Callback, onLoad: List[A => Callback]) extends State[A]
    final case class Loading[A](onLoad: List[A => Callback]) extends State[A]
    final case class Loaded[A](value: A) extends State[A]
  }

  implicit val scalazInstance: Applicative[LazyLoader] =
    new Applicative[LazyLoader] {
      override def point[A](a: => A) = LazyLoader.sync(a)
      override def map[A, B](fa: LazyLoader[A])(f: A => B) = fa map f
      override def ap[A, B](la: => LazyLoader[A])(lf: => LazyLoader[A => B]): LazyLoader[B] =
        LazyLoader.async[B] { complete =>
          val pf = Promise[A => B]()
          val pa = Promise[A]()
          val fb: Future[B] =
            for {
              f <- pf.future
              a <- pa.future
            } yield f(a)
          fb.onComplete(_.foreach(complete(_).runNow()))
          for {
            _ <- la.load(a => Callback(pa.success(a)))
            _ <- lf.load(f => Callback(pf.success(f)))
          } yield ()
        }
    }
}
