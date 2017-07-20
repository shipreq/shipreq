package shipreq.webapp.server.protocol

import java.nio.ByteBuffer
import net.liftweb.common.{Empty, Full, Failure => BoxFailure}
import net.liftweb.http.{BadRequestResponse, InternalServerErrorResponse, LiftResponse, S}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.protocol._
import shipreq.webapp.server.logic.Server._

object ServerProtocol extends HasLogger {

  def registerServerSideProc(localFn: ByteBuffer => Fx[ProtocolError \/ ByteBuffer]): ServerSideProcId = {

    val onProtocolError: ProtocolError => LiftResponse = {
      case RequestPickleError(_) => BadRequestResponse()
      case ResponsePickleError(e) =>
//        log.error(e, s"Error responding to $p with $r")
        InternalServerErrorResponse()
    }

    val proc = S.NFuncHolder { () =>
      @inline implicit def autoL[A](r: LiftResponse): T[A] = -\/(r)
      @inline implicit def autoR[A](a: A): T[A] = \/-(a)
      type T[A] = LiftResponse \/ A

      // TODO log errors to support

      def readReqBody: T[Array[Byte]] =
        S.request.flatMap(_.body) match {
          case Full(body)    => body
          case Empty         => BadRequestResponse()
          case e: BoxFailure =>
//            log.error(s"Error reading $p request: $e")
            BadRequestResponse()
        }

      (for {
        in <- readReqBody
        out <- localFn(ByteBuffer wrap in).unsafeRun().leftMap(onProtocolError)
      } yield BinaryResponse(out))
        .merge[LiftResponse]
    }

    val fnName = S.formFuncName
    S.addFunctionMap(fnName, proc)
    ServerSideProcId(fnName)
  }
}
