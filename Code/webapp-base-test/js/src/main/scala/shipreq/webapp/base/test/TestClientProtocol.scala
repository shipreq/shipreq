package shipreq.webapp.base.test

import japgolly.scalajs.react.Callback
import org.scalajs.dom.console
import scalaz.{-\/, Equal, \/, \/-}
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.protocol._
//import TestClientProtocol._

//object TestClientProtocol {
//  trait Req {
//    val proc      : ServerSideProc[_, _]
//    val input     : proc.protocol.Input
//    val onResponse: Throwable \/ proc.protocol.Output => Callback
//
//    override def toString =
//      "Req[%08X]:%s(%s)".format(##, proc.id.value.trim, input)
//
//    def force[I, O](p2: ServerSideProc[I, O]) = {
////      assertEq[ServerSideProc.Protocol[_, _]](proc.protocol, p2.protocol)
//      assert(proc == p2)
//      this.asInstanceOf[Req {val proc: p2.type}]
//    }
//
//    def forceP[I, O](p2: ServerSideProc.Protocol[I, O]) = {
//      assert(proc.protocol == p2)
//      forceIO[I, O]
//    }
//
//    def forceIO[I, O] =
//      this.asInstanceOf[ReqP[I, O]]
//
//    var _pendingResponse = true
//    def responsePending = _pendingResponse
//    def responded = !responsePending
//
//    def markResponded(): Unit =
//      if (responsePending)
//        _pendingResponse = false
//      else
//        sys error "Request has already been responded to."
//  }
//  type ReqP[I, O] = Req { val proc: ServerSideProc[I, O] }
//}
//
//class TestClientProtocol(autoRespondArg: Boolean) extends ClientProtocol {
//
//  var reqs = Vector.empty[Req]
//
//  def reset(): Unit =
//    reqs = Vector.empty
//
//  var autoRespond: Boolean =
//    autoRespondArg
//
//  var autoResponsePFs: List[PartialFunction[Req, Callback]] =
//    Nil
//
//  var autoResponseFallback: Req => Callback =
//    r => Callback(console.warn(s"${Console.YELLOW}Don't know how to respond to $r${Console.RESET}"))
//
//  final def autoResponse(r: Req): Callback =
//    (autoResponsePFs.find(_.isDefinedAt(r)).getOrElse(autoResponseFallback))(r)
//
//  def addAutoResponsePF(f: PartialFunction[Req, Callback]): Unit =
//    autoResponsePFs :+= f
//
//  def addAutoResponse[I, O](p: ServerSideProc.Protocol[I, O])(f: ReqP[I, O] => Callback): Unit =
//    addAutoResponsePF {
//      case r if r.proc.protocol ==* p => f(r.asInstanceOf[ReqP[I, O]])
//    }
//
//  override def call[I, O](_proc      : ServerSideProc[I, O])
//                         (_input     : I,
//                          _onResponse: Throwable \/ O => Callback): Callback = {
//    //println(s"RPC: ${_r.d}(${_r.n}) ← ${_i}")
//    Callback {
//      val r = new Req {
//        override val proc: _proc.type = _proc
//        override val input = _input
//        override val onResponse = _onResponse
//      }
//      reqs :+= r
//      if (autoRespond)
//        autoRespondToLast()
//    }
//  }
//
//  def assertReqsSent(count: Int): Unit =
//    assertEq("AJAX requests", reqs.size, count)
//
//  def last = reqs.last
//
//  def respondToLast[I, O](p: ServerSideProc[I, O])(o: O): Unit =
//    last.force(p).onResponse(\/-(o)).runNow()
//
//  def respondToLastP[I, O](p: ServerSideProc.Protocol[I, O])(o: O): Unit =
//    last.forceP(p).onResponse(\/-(o)).runNow()
//
//  def autoRespondToLast(): Unit = {
//    val r = last
//    r.markResponded()
//    autoResponse(r).runNow()
//  }
//
//  def failLast(): Unit = {
//    val r = last
//    r.markResponded()
//    r.onResponse(-\/(new Throwable("Dummy error from TestClientProtocol.failLast()"))).runNow()
//  }
//
//  def lastTwo[I, O](r: ServerSideProc[I, O]) = {
//    val Vector(a, b) = reqs.takeRight(2).map(_.force(r))
//    (a, b)
//  }
//
//  def assertLastTwoRequestsEqual[I, O](p: ServerSideProc[I, O])(implicit e: Equal[I]): Unit = {
//    val (a, b) = lastTwo(p)
//    assertEq("Last two requests", a.input, b.input)
//  }
//}
