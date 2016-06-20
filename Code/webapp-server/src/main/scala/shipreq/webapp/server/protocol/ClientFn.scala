package shipreq.webapp.server.protocol

import boopickle._
import java.nio.ByteBuffer
import java.util.Base64
import shipreq.base.util.Util.quickSB
import shipreq.webapp.base.protocol.ClientFnDecl
import shipreq.webapp.server.snippet.Asset
import ClientFn.binaryToBase64

final class ClientFn[I](decl: ClientFnDecl[I]) {
  import decl.pickler

  private def runOnWindowLoad(f: StringBuilder => Unit): StringBuilder => Unit =
    sb => {
      sb append "window.onload=function(){"
      f(sb)
      sb append "};"
    }

  private def setTimeout(ms: Int)(f: StringBuilder => Unit): StringBuilder => Unit =
    sb => {
      sb append "setTimeout(function(){"
      f(sb)
      sb append "},"
      sb append ms
      sb append ");"
    }

  private def loadAsyncThenWhenReady(assets: Asset.InitAndNext[_], name: String = "x")
                                    (onReady: StringBuilder => Unit): StringBuilder => Unit =
    sb => {
      sb append "loadjs("
      sb append assets.nextPathsArray
      sb append ",'"
      sb append name
      sb append "');loadjs.ready('"
      sb append name
      sb append "',{success:function(){"
      onReady(sb)
      sb append "}});"
    }

  private val runCmdHead =
    s"${decl.objectName}().${decl.methodName}('"

  private def run(i: I)(sb: StringBuilder): Unit = {
    sb append runCmdHead
    sb append binaryToBase64(PickleImpl.intoBytes(i))
    sb append "');"
  }

  def runOnLoad(i: I): String =
    quickSB(
      runOnWindowLoad(
        run(i)))

  def loadJsAndRun(assets: Asset.InitAndNext[_], name: String = "x")(i: I): String =
    quickSB(
      runOnWindowLoad(
        loadAsyncThenWhenReady(assets, name)(
          run(i))))

  def htmlToRunOnLoad(i: I) =
    <script type="text/javascript">{runOnLoad(i)}</script>

  def htmlToLoadJsAndRun(assets: Asset.InitAndNext[_], name: String = "x")(i: I) =
    <script type="text/javascript">{loadJsAndRun(assets, name)(i)}</script>
}

object ClientFn {

  def binaryToBase64(bb: ByteBuffer): String = {
    val size = bb.limit()
    val a = new Array[Byte](size)
    bb.array().copyToArray(a, 0, size)
    Base64.getEncoder.encodeToString(a)
  }

  val HomeSpa    = new ClientFn(ClientFnDecl.HomeSpa)
  val ProjectSpa = new ClientFn(ClientFnDecl.ProjectSpa)
}

