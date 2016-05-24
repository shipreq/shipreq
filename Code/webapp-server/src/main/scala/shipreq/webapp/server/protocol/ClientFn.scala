package shipreq.webapp.server.protocol

import boopickle._
import java.nio.ByteBuffer
import java.util.Base64
import shipreq.base.util.Util.quickSB
import shipreq.webapp.base.protocol.ClientFnDecl
import ClientFn.binaryToBase64

final class ClientFn[I](decl: ClientFnDecl[I]) {
  import decl.pickler

  @inline private def runOnWindowLoad(f: StringBuilder => Unit): StringBuilder => Unit =
    sb => {
      sb append "window.onload=function(){"
      f(sb)
      sb append "};"
    }

  private val invokeLead =
    s"${decl.objectName}().${decl.methodName}('"

  def runOnLoadJs(i: I): String =
    quickSB(
      runOnWindowLoad { sb =>
        sb append invokeLead
        sb append binaryToBase64(PickleImpl.intoBytes(i))
        sb append "');"
      }
    )

  def runOnLoadHtml(i: I) =
    <script type="text/javascript">{runOnLoadJs(i)}</script>
}

object ClientFn {

  def binaryToBase64(bb: ByteBuffer): String = {
    val size = bb.limit()
    val a = new Array[Byte](size)
    bb.array().copyToArray(a, 0, size)
    Base64.getEncoder.encodeToString(a)
  }

  val ProjectSpa = new ClientFn(ClientFnDecl.ProjectSpa)

}

