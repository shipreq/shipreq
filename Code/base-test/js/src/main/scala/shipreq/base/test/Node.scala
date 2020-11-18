package shipreq.base.test

import japgolly.scalajs.react.AsyncCallback
import org.scalajs.dom.crypto.Crypto
import scala.concurrent.Future
import scala.scalajs.js
import shipreq.base.test.BaseTestUtil._

/** Node JS access provided by `project/AdvancedNodeJSEnv.scala`.
 */
object Node {

  @inline private def window = js.Dynamic.global.window
  @inline private def node = window.node

  private def require(path: String): js.Dynamic =
    node.require(path).asInstanceOf[js.Dynamic]

  private def envVar(name: String): js.UndefOr[String] =
    node.process.env.selectDynamic(name).asInstanceOf[js.UndefOr[String]]

  private def envVarNeed(name: String): String =
    envVar(name).getOrElse(throw new RuntimeException("Missing env var: " + name))

  private val inCI             = envVar("CI").contains("1")
  private val sbtRootDir       = envVarNeed("SBT_ROOT")
  private val testNodeDir      = sbtRootDir + "/frontend/dist/test-node"
  private val asyncTestTimeout = if (inCI) 60000 else 3000

  val loadFakeIndexedDb: () => Unit =
    onceUnit {
      require(s"$testNodeDir/fake-indexeddb")
      js.Dynamic.global.window.indexedDB   = node.indexedDB
      js.Dynamic.global.window.IDBKeyRange = node.IDBKeyRange
    }

  def asyncTest[A](ac: AsyncCallback[A]): Future[A] = {
    ac.timeoutMs(asyncTestTimeout).map {
      case Some(a) => a
      case None    => fail(s"Async test timed out after ${asyncTestTimeout / 1000} sec.")
    }.unsafeToFuture()
  }

  lazy val webCrypto: Crypto = {
    // https://github.com/nodejs/node/pull/35093
    val c = require("crypto").webcrypto.asInstanceOf[Crypto]
    window.crypto = c // TODO remove after https://github.com/scala-js/scala-js-dom/pull/432
    c
  }
}
