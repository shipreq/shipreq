package shipreq.webapp.client.project.test

import boopickle.Pickler
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Success, Try}
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.ww.api.WebWorkerCmd
import shipreq.webapp.client.ww.api.WebWorkerCmd.NoResult
import shipreq.webapp.member.project.data.Svg

final class TestWebWorkerClient(initialPrep: TestWebWorkerClient.Prep,
                                logger     : LoggerJs) extends WebWorkerClient.Instance {
  import TestWebWorkerClient._

  private var responses = Vector.empty[(WebWorkerCmd[_], Int) => Option[Any]]
  private var requests  = Vector.empty[WebWorkerCmd[_]]
  private var pending   = Vector.empty[Pending]

  override def close: Callback =
    Callback.empty

  override def encode(cmd: WebWorkerCmd[_]): ArrayBuffer =
    null

  override def sendEncoded[A](cmd: WebWorkerCmd[A], enc: ArrayBuffer)(implicit readResult: Pickler[A]): AsyncCallback[A] =
    AsyncCallback.byName {
      val id = requests.length
      requests :+= cmd
      logger(_.info(s"WW received request #${id + 1}: ${cmd.toString.quoteInner.take(100)}"))
      responses.iterator.map(_ (cmd, id)).filterDefined.nextOption() match {
        case Some(r) =>
          logger(_.info(s"  Returning user-specified response: ${("" + r).quoteInner.take(100)}"))
          AsyncCallback.delay(r.asInstanceOf[A])
        case None =>
          logger(_.info("  No user-specified response. Trying fallbacks..."))
          fallbackResponse(id, cmd)
      }
    }

  private def fallbackResponse[A](id: Int, cmd: WebWorkerCmd[A]): AsyncCallback[A] =
    cmd match {
      case _: WebWorkerCmd.Init
         | _: WebWorkerCmd.UpdateProject =>
        logger(_.info("  Responding with NoResult."))
        AsyncCallback.pure(NoResult)

      case _: WebWorkerCmd.GraphUseCaseFlow
         | _: WebWorkerCmd.GraphReqImplications
         | _: WebWorkerCmd.GraphAllImplications
         | _: WebWorkerCmd.GraphInline =>
        logger(_.info("  Stashing."))
        AsyncCallback.promise[A].asAsyncCallback.flatMap { case (promise, complete) =>
          val remove = Callback {pending = pending.filterNot(_.id == id)}
          val p = Pending(id, cmd, t => remove >> complete(t.asInstanceOf[Try[A]]))
          pending :+= p
          promise
        }
    }

  def respondTo[A](cmd: WebWorkerCmd[A]) =
    new RespondDsl[A]((c, _) => c == cmd)

  def respondWhen(cond: WebWorkerCmd[_] => Boolean) =
    new RespondDsl[Any]((c, _) => cond(c))

  def respondToNext = {
    val nextId = requests.length
    new RespondDsl[Any]((_, id) => id == nextId)
  }

  def respondToAll =
    new RespondDsl[Any]((_, _) => true)

  def respondToAllGraphsWith(svg: Svg): Unit =
    respondWhen(isGraph)(\/-(svg))

  final class RespondDsl[A](cond: (WebWorkerCmd[_], Int) => Boolean) {
    def apply(response: => A): Unit =
      by(_ => response)

    def by(respond: WebWorkerCmd[_] => A): Unit = {
      val f = (cmd: WebWorkerCmd[_], id: Int) => Option.when(cond(cmd, id))(respond(cmd))
      responses :+= f
      val pendingNow = pending
      for {
        p <- pendingNow
        r <- f(p.cmd, p.id)
      } yield {
        logger(_.info(s"Completing WW request #${p.id + 1} with ${("" + r).quoteInner.take(100)}"))
        p.complete(Success(r))
      }
    }
  }

//  def requestCount() = requests.length
//  def lastRequest()  = requests.lastOption

  initialPrep(this)
}

object TestWebWorkerClient {

  type Prep = TestWebWorkerClient => Unit

  val noInitialPrep: Prep =
    _ => ()

  val defaultLogger =
    LoggerJs.off

  def apply(initialPrep: Prep     = noInitialPrep,
            logger     : LoggerJs = defaultLogger,
           ): TestWebWorkerClient =
    new TestWebWorkerClient(initialPrep, logger)

  // ===================================================================================================================

  private final case class Pending(id      : Int,
                                   cmd     : WebWorkerCmd[_],
                                   complete: Try[_] => Callback)

  private val isGraph: WebWorkerCmd[_] => Boolean = {
    case _: WebWorkerCmd.Init
       | _: WebWorkerCmd.UpdateProject => false
    case _: WebWorkerCmd.GraphUseCaseFlow
       | _: WebWorkerCmd.GraphReqImplications
       | _: WebWorkerCmd.GraphAllImplications
       | _: WebWorkerCmd.GraphInline => true
  }

  // ===================================================================================================================

//  final class Obs(ww: TestWebWorkerClient) {
//    val requestCount = ww.requestCount()
//    val lastRequest  = ww.lastRequest()
//  }

  import shipreq.webapp.base.test.TestState._

  final class TestDsl[R, O, S](val dsl: Dsl[Id, R, O, S, String])
                              (getRef : R => TestWebWorkerClient,
//                             getObs : O => Obs,
                              ) {
    private implicit def autoRef(r: R) = getRef(r)
//  private implicit def autoObs(o: O) = getObs(o)

    def prepare(f: Prep): dsl.Actions =
      prepare("Prepare TestWebWorkerClient", f)

    def prepare(desc: String, f: Prep): dsl.Actions =
      dsl.action(desc)(x => f(x.ref))
  }
}
