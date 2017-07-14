package shipreq.webapp.server.protocol

import boopickle._
import java.nio.ByteBuffer
import net.liftweb.common.{Empty, Full, Failure => BoxFailure}
import net.liftweb.http.{BadRequestResponse, InternalServerErrorResponse, LiftResponse, S}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.Fx._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.protocol.ServerSideProc

object ServerProtocol extends HasLogger {

  def createServerSideProc(p: ServerSideProc.Protocol)(localFn: p.Input => Fx[p.Response]): p.Instance = {
    import p._

    val proc = S.NFuncHolder { () =>
      type T[A] = LiftResponse \/ A
      @inline implicit def autoL[A](r: LiftResponse): T[A] = -\/(r)
      @inline implicit def autoR[A](a: A): T[A] = \/-(a)

      // TODO log errors to support

      def readReqBody: T[Array[Byte]] =
        S.request.flatMap(_.body) match {
          case Full(body)    => body
          case Empty         => BadRequestResponse()
          case e: BoxFailure =>
            log.error(s"Error reading $p request: $e")
            BadRequestResponse()
        }

      def unpickle(b: Array[Byte]): T[Input] =
        try {
          UnpickleImpl(pickleInput).fromBytes(ByteBuffer wrap b)
        } catch {
          case e: Throwable => BadRequestResponse()
        }

      def process(i: Input): T[Response] =
        try {
          localFn(i).unsafeRun()
        } catch {
          case e: Throwable =>
            log.error(e, s"Error processing $p request $i")
            InternalServerErrorResponse()
        }

      def sendResponse(r: Response): T[LiftResponse] =
        try {
          val binary = PickleImpl.intoBytes(r)
          BinaryResponse(binary)
        } catch {
          case e: Throwable =>
            log.error(e, s"Error responding to $p with $r")
            InternalServerErrorResponse()
        }

//      val r = readReqBody flatMap unpickle flatMap process flatMap sendResponse
      val r = readReqBody.flatMap(b => unpickle(b).flatMap(i => process(i).flatMap(o => sendResponse(o))))
      r.merge[LiftResponse]
    }

    val fnName = S.formFuncName
    S.addFunctionMap(fnName, proc)

    ServerSideProc(fnName, p)
  }
}
