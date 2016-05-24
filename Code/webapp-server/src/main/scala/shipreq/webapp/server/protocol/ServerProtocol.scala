package shipreq.webapp.server.protocol

import boopickle._
import java.nio.ByteBuffer
import net.liftweb.common.{Empty, Failure => BoxFailure, Full}
import net.liftweb.http.{BadResponse, InternalServerErrorResponse, LiftResponse, S}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.protocol.RemoteFn

object ServerProtocol extends HasLogger {

  def remoteFn(fn: RemoteFn)(localFn: fn.Input => fn.Response): fn.Instance = {
    import fn._

    val proc = S.NFuncHolder { () =>
      type T[A] = LiftResponse \/ A
      @inline implicit def autoL[A](r: LiftResponse): T[A] = -\/(r)
      @inline implicit def autoR[A](a: A): T[A] = \/-(a)

      // TODO log errors to support

      def readReqBody: T[Array[Byte]] =
        S.request.flatMap(_.body) match {
          case Full(body)    => body
          case Empty         => BadResponse()
          case e: BoxFailure =>
            log.error(s"Error reading $fn request: $e")
            BadResponse()
        }

      def unpickle(b: Array[Byte]): T[Input] =
        try {
          UnpickleImpl(pickleInput).fromBytes(ByteBuffer wrap b)
        } catch {
          case e: Throwable => BadResponse()
        }

      def process(i: Input): T[Response] =
        try {
          localFn(i)
        } catch {
          case e: Throwable =>
            log.error(s"Error processing $fn request $i: $e")
            InternalServerErrorResponse()
        }

      def sendResponse(r: Response): T[LiftResponse] =
        try {
          val binary = PickleImpl.intoBytes(r)
          BinaryResponse(binary)
        } catch {
          case e: Throwable =>
            log.error(s"Error responding to $fn with $r: $e")
            InternalServerErrorResponse()
        }

//      val r = readReqBody flatMap unpickle flatMap process flatMap sendResponse
      val r = readReqBody.flatMap(b => unpickle(b).flatMap(i => process(i).flatMap(o => sendResponse(o))))
      r.merge[LiftResponse]
    }

    val fnName = S.formFuncName
    S.addFunctionMap(fnName, proc)

    RemoteFn.Instance(fnName, fn)
  }
}
