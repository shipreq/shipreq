package shipreq.webapp.base.test

import boopickle.Pickler
import japgolly.scalajs.react.{AsyncCallback, Callback}
import org.scalajs.dom.console
import scala.util.{Failure, Success}
import scalaz.{-\/, \/, \/-}
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.protocol2._

object TestAjaxClient {

  trait Req {
    val ajax      : Protocol.Ajax[Pickler]
    val input     : ajax.protocol.RequestType
    val onResponse: Throwable \/ ajax.protocol.ResponseType => Callback

    override def toString =
      "Req[%08X]:%s(%s)".format(##, ajax.url, input)

    def force(p2: Protocol.Ajax[Pickler]) = {
      assert(ajax eq p2)
      this.asInstanceOf[ReqOf[p2.type]]
    }

    private var _pendingResponse = true
    def responsePending = _pendingResponse
    def responded = !responsePending

    def markResponded(): Unit =
      if (responsePending)
        _pendingResponse = false
      else
        sys error "Request has already been responded to."
  }

  type ReqOf[P <: Protocol.Ajax[Pickler]] = Req {val ajax: P}
}

class TestAjaxClient(autoRespondArg: Boolean) extends AjaxClient.Binary {
  import TestAjaxClient._

  var reqs = Vector.empty[Req]

  def reset(): Unit =
    reqs = Vector.empty

  var autoRespond: Boolean =
    autoRespondArg

  var autoResponsePFs: List[PartialFunction[Req, Callback]] =
    Nil

  var autoResponseFallback: Req => Callback =
    r => Callback(console.warn(s"${Console.YELLOW}Don't know how to respond to $r${Console.RESET}"))

  final def autoResponse(r: Req): Callback =
    autoResponsePFs.find(_.isDefinedAt(r)).getOrElse(autoResponseFallback)(r)

  def addAutoResponsePF(f: PartialFunction[Req, Callback]): Unit =
    autoResponsePFs :+= f

  def addAutoResponse(p: Protocol.Ajax[Pickler])(f: ReqOf[p.type] => Callback): Unit =
    addAutoResponsePF {
      case r if r.ajax eq p => f(r.force(p))
    }

  override def apply(p: Protocol.Ajax[Pickler])
                    (req: p.protocol.RequestType): AsyncCallback[p.protocol.ResponseType] =
    for {
      (promise, complete) <- AsyncCallback.promise[p.protocol.ResponseType].asAsyncCallback

      _ <- AsyncCallback.point {
        val r = new Req {
          override val ajax: p.type = p
          override val input = req
          override val onResponse = {
            case \/-(o) => complete(Success(o))
            case -\/(t) => complete(Failure(t))
          }
        }
        reqs :+= r
        if (autoRespond)
          autoRespondToLast()
      }

      result <- promise
    } yield result

  def assertReqsSent(count: Int): Unit =
    assertEq("AJAX requests", reqs.size, count)

  def last = reqs.last

  def respondToLast(p: Protocol.Ajax[Pickler])(o: p.protocol.ResponseType): Unit =
    last.force(p).onResponse(\/-(o)).runNow()

  def autoRespondToLast(): Unit = {
    val r = last
    r.markResponded()
    autoResponse(r).runNow()
  }

  def failLast(): Unit = {
    val r = last
    r.markResponded()
    r.onResponse(-\/(new Throwable("Dummy error from TestAjaxClient.failLast()"))).runNow()
  }

//  def lastTwo[I, O](r: ServerSideProc[I, O]) = {
//    val Vector(a, b) = reqs.takeRight(2).map(_.force(r))
//    (a, b)
//  }
//
//  def assertLastTwoRequestsEqual[I, O](p: ServerSideProc[I, O])(implicit e: Equal[I]): Unit = {
//    val (a, b) = lastTwo(p)
//    assertEq("Last two requests", a.input, b.input)
//  }
}
