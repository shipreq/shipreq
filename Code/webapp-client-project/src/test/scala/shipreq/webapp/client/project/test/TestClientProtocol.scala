package shipreq.webapp.client.project.test

import japgolly.scalajs.react.Callback
import scalaz.{-\/, Equal}
import scalaz.std.AllInstances._
import shipreq.webapp.base.protocol.RemoteFn
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.base.protocol.ClientProtocol.Failed
import shipreq.webapp.client.project.test.TestUtil._

object TestClientProtocol {
  trait Req {
    val r      : RemoteFn.Instance
    val input  : r.fn.Input
    val success: r.fn.Output => TCB.Success
    val failure: Failed[r.fn.Failure] => TCB.Failure

    override def toString =
      s"Req($input)@${Integer.toHexString(##)}"

    def force(r2: RemoteFn.Instance) =
      this.asInstanceOf[Req {val r: r2.type}]

    var _pendingResponse = true
    def responsePending = _pendingResponse
    def responded = !responsePending

    def markResponded(): Unit =
      if (responsePending)
        _pendingResponse = false
      else
        sys error "Request has already been responded to."
  }
}

import TestClientProtocol.Req

class TestClientProtocol extends ClientProtocol {

  var reqs = Vector.empty[Req]

  def reset(): Unit =
    reqs = Vector.empty

  var autoRespond = true

  def autoResponse(r: Req): Callback =
    Callback.empty

  def call(i: RemoteFn.Instance)(_input  : i.fn.Input,
                                 _success: i.fn.Output => TCB.Success,
                                 _failure: Failed[i.fn.Failure] => TCB.Failure): Callback = {
    //println(s"RPC: ${_r.d}(${_r.n}) ← ${_i}")
    Callback {
      val r = new Req {
        override val r: i.type = i
        override val input   = _input
        override val success = _success
        override val failure = _failure
      }
      reqs :+= r
      if (autoRespond)
        autoRespondToLast()
    }
  }

  def assertReqsSent(count: Int): Unit =
    assertEq("AJAX requests", reqs.size, count)

  def last = reqs.last

  def respondToLast(r: RemoteFn.Instance)(o: r.fn.Output): Unit =
    last.force(r).success(o).runNow()

  def autoRespondToLast(): Unit = {
    val r = last
    r.markResponded()
    autoResponse(r).runNow()
  }

  def failLast(): Unit = {
    val r = last
    r.markResponded()
    r.failure(-\/(new Throwable("Dummy error from TestClientProtocol.failLast()"))).runNow()
  }

  def lastTwo(r: RemoteFn.Instance) = {
    val Vector(a, b) = reqs.takeRight(2).map(_.force(r))
    (a, b)
  }

  def assertLastTwoRequestsEqual(r: RemoteFn.Instance)(implicit e: Equal[r.fn.Input]): Unit = {
    val (a, b) = lastTwo(r)
    assertEq("Last two requests", a.input, b.input)
  }
}
