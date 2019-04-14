package shipreq.webapp.server.protocol2

import boopickle._
import java.lang.StringBuilder
import java.nio.ByteBuffer
import java.util.Base64
import net.liftweb.http.js.{JsCmd, JsCmds}
import shipreq.base.util.Util.quickJSB
import shipreq.webapp.base.protocol2._

/**
  * Generates code to invoke client-side procedures.
  */
final class ClientSideProcInvoker[I](proc: ClientSideProc[I], bundle: Option[LoadJs.Bundle]) {
  import ClientSideProcInvoker._
  import proc.pickler

  private val runCmdHead =
    s"${proc.objectAndMethod}('"

  private def invokeSB(i: I)(sb: StringBuilder): Unit = {
    sb append runCmdHead
    byteBufferToBase64SB(PickleImpl.intoBytes(i))(sb)
    sb append "')"
  }

  private def invokeJs(i: I): String =
    quickJSB(invokeSB(i))

  def invokeJsCmd(i: I): JsCmd =
    JsCmds.Run(invokeJs(i))

  // TODO Potential optimisation: have this estimate a good initial SB size for itself by observing past results
  private val invokeOnLoadJs: I => String =
      bundle match {
        case Some(loadjs) =>
          i => {
            val js = quickJSB(invokeSB(i))
            loadjs.onSuccess(js)
          }
        case None =>
          i => quickJSB { sb =>
            sb append "window.onload=function(){"
            invokeSB(i)(sb)
            sb append "};"
          }
      }

  def invokeOnLoadHtml(i: I) =
    <script type="text/javascript">{invokeOnLoadJs(i)}</script>
}

object ClientSideProcInvoker {

  def apply[I](proc: ClientSideProc[I]): ClientSideProcInvoker[I] =
    new ClientSideProcInvoker(proc, None)

  def apply[I](proc: ClientSideProc[I], bundle: LoadJs.Bundle): ClientSideProcInvoker[I] =
    new ClientSideProcInvoker(proc, Some(bundle))

  def byteBufferToBinary(bb: ByteBuffer): Array[Byte] = {
    val size = bb.limit()
    val a = new Array[Byte](size)
    bb.array().copyToArray(a, 0, size)
    a
  }

  def byteBufferToBase64String(bb: ByteBuffer): String =
    Base64.getEncoder.encodeToString(byteBufferToBinary(bb))

  def byteBufferToBase64SB(bb: ByteBuffer)(sb: StringBuilder): Unit = {
    val b64 = Base64.getEncoder.encode(byteBufferToBinary(bb))
    var i = 0
    while (i < b64.length) {
      sb.append(b64(i).toChar)
      i += 1
    }
  }
}

