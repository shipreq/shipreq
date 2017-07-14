package shipreq.webapp.server.protocol

import boopickle._
import java.lang.StringBuilder
import java.nio.ByteBuffer
import java.util.Base64
import net.liftweb.http.js.{JsCmd, JsCmds}
import shipreq.base.util.Util.quickJSB
import shipreq.webapp.base.protocol._

/**
  * Generates code to invoke client-side procedures.
  */
final case class ClientSideProcInvoker[I](proc: ClientSideProc[I]) {
  import ClientSideProcInvoker._
  import proc.pickler

  private def onWindowLoadSB(f: StringBuilder => Unit): StringBuilder => Unit =
    sb => {
      sb append "window.onload=function(){"
      f(sb)
      sb append "};"
    }

//  private def setTimeout(ms: Int)(f: StringBuilder => Unit): StringBuilder => Unit =
//    sb => {
//      sb append "setTimeout(function(){"
//      f(sb)
//      sb append "},"
//      sb append ms
//      sb append ");"
//    }

//  private def loadAsyncThenWhenReady(assets: Asset.InitAndNext[_], name: String = "x")
//                                    (onReady: StringBuilder => Unit): StringBuilder => Unit =
//    sb => {
//      sb append "loadjs("
//      sb append assets.nextPathsArray
//      sb append ",'"
//      sb append name
//      sb append "');loadjs.ready('"
//      sb append name
//      sb append "',{success:function(){"
//      onReady(sb)
//      sb append "}});"
//    }

  private val runCmdHead =
    s"${proc.objectAndMethod}('"

  private def invokeSB(i: I)(sb: StringBuilder): Unit = {
    sb append runCmdHead
    byteBufferToBase64SB(PickleImpl.intoBytes(i))(sb)
    sb append "');"
  }

  def invokeJs(i: I): String =
    quickJSB(invokeSB(i))

  def invokeJsCmd(i: I): JsCmd =
    JsCmds.Run(invokeJs(i))

  def invokeOnLoadJs(i: I): String =
    quickJSB(onWindowLoadSB(invokeSB(i)))

  def invokeOnLoadHtml(i: I) =
    <script type="text/javascript">{invokeOnLoadJs(i)}</script>

  //  def loadJsAndRun(assets: Asset.InitAndNext[_], name: String = "x")(i: I): String =
//    quickSB(
//      runOnWindowLoad(
//        loadAsyncThenWhenReady(assets, name)(
//          run(i))))

//  def htmlToLoadJsAndRun(assets: Asset.InitAndNext[_], name: String = "x")(i: I) =
//    <script type="text/javascript">{loadJsAndRun(assets, name)(i)}</script>
}

object ClientSideProcInvoker {

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

