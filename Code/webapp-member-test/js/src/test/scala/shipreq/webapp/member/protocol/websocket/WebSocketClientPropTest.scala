package shipreq.webapp.member.project.protocol.websocket

import nyaya.gen._
import scala.collection.mutable.ArrayBuffer
import shipreq.base.util._
import shipreq.webapp.base.protocol.websocket.WebSocket.ReadyState
import shipreq.webapp.base.protocol.websocket.WebSocketShared.CloseCode
import shipreq.webapp.member.test.WebappTestUtil._
import utest._

object WebSocketClientPropTest extends TestSuite {
  import WebSocketClientTester.debug

  private final case class Possibility(name: String, gen: Gen[Any])

  private val closeCodes = Vector[CloseCode](
    CloseCode.cannotAccept,
    CloseCode.closedAbnormally,
    CloseCode.normalClosure,
    CloseCode.noStatusCode,
    CloseCode.protocolError,
    CloseCode.respondException,
    CloseCode.serviceRestart,
    CloseCode.tryAgainLater,
    CloseCode.unauthorised,
    CloseCode.unauthorised,
    CloseCode.unauthorised,
    CloseCode.unauthorised,
    CloseCode.unauthorised,
    CloseCode.unauthorised,
    CloseCode.unauthorised,
    CloseCode.unexpectedCondition,
    CloseCode.unhandledException,
  )

  protected def test(steps: Int): Unit =
    test(steps, None)

  protected def test(steps: Int, explicitSeed: Long): Unit =
    test(steps, Some(explicitSeed))

  private def test(steps: Int, explicitSeed: Option[Long]): Unit = {
    val tester = WebSocketClientTester()
    import tester._

    val constPossibilities = List[Possibility](
      Possibility("client.connect", Gen.delay(client.connect.runNow())),
      Possibility("client.close", Gen.delay(client.close.runNow())),
      Possibility("client.keepAlive", Gen.delay(client.keepAlive.runNow())),
      Possibility("user.sendMsg", Gen.delay(sendMsg())),
    )

    val wsOpen     = Possibility("ws.open", Gen.delay(ws().open()))
    val wsClosing  = Possibility("ws.closing", Gen.delay(ws().closing()))
    val wsClose    = Possibility("ws.close", Gen.choose_!(closeCodes).map(ws().close(_)))
    val wsPush     = Possibility("ws.push", Gen.delay(server.push()))

    implicit val ctx = GenCtx(GenSize(32), ThreadNumber(1))
    val seed = explicitSeed.getOrElse(Gen.long.samplesUsing(ctx).next())
    ctx.setSeed(seed)

    nextReauthResult = {
      val results = Gen.boolean.map(Allow.when).samplesUsing(ctx)
      () => results.next()
    }

    var stepRecord = Vector.empty[String] // yeah, i'm lazy lol

    def step(stepNo: Int): Unit = {
      val possibilities = ArrayBuffer.empty[() => Possibility]

      def add(p: => Possibility): Unit =
        possibilities += (() => p)

      def addGen(name: String, g: => Gen[Any]): Unit =
        add(Possibility(name, g))

      def addProc(name: String, f: => Any): Unit =
        addGen(name, Gen.delay(f))

      // ---------------------------------------------------------------------------------------------------------------
      // Possibilities

      constPossibilities.foreach(add(_))

      if (timers.nonEmpty) addProc("timers.runNext", timers.runNext().foreach(_.get))

      webSockets.lastOption.map(_.readyState()) match {
        case Some(ReadyState.Connecting) =>
          add(wsOpen)
          add(wsClosing)
          add(wsClose)

        case Some(ReadyState.Open) =>
          add(wsClosing)
          add(wsClose)
          add(wsPush)
          if (ws().pendingResponse().nonEmpty) addProc("ws.respond", server.respondToNextPending())

        case Some(ReadyState.Closing) =>
          add(wsClose)

        case Some(ReadyState.Closed) =>

        case None =>
      }

      // ---------------------------------------------------------------------------------------------------------------

      val plan =
        for {
          i <- Gen.chooseInt(possibilities.length)
          p  = possibilities(i)()
          _  = stepRecord :+= p.name
          _  = if (debug) println(s"Step $stepNo: ${p.name}")
          a <- p.gen
        } yield a

      plan.samplesUsing(ctx).next()

      checkInvariants()
    }

    try {
      checkInvariants()

      var i = 0
      while (i < steps) {
        i += 1
        step(i)
      }

      webSockets.lastOption.foreach(_.close())

      // Final checks

      val badIndices = sendResults.indices.iterator.filter(sendResults(_).length != 1).toList
      for (i <- badIndices) {
        val r = sendResults(i)
        println(s"Request #$i has ${r.length} results: ${r.mkString(", ")}")
      }
      assertEq("All requests should have 1 response", sendResults.map(_.length), Vector.fill(sendResults.length)(1))

    } catch {
      case t: Throwable =>
        println(s"WebSocketClientPropTest seed = ${seed}L\n")
        throw t
    }
  }

  override def tests = Tests {
    "prop" - test(999)
  }
}
