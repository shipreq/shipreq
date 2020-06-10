package shipreq.webapp.server.test

import java.io.File
import net.liftweb.common.Logger
import net.liftweb.util.TimeHelpers._
import org.eclipse.jetty.server._
import org.eclipse.jetty.webapp.WebAppContext
import org.apache.commons.io.FileUtils
import shipreq.base.util.ThreadUtils
import shipreq.webapp.base.AssetManifest

object TestJetty extends Jetty(8090)

/**
 * Starts up an instance of Jetty than runs the webapp.
 *
 * @since 29/04/2013
 */
class Jetty(val port: Int) extends Logger {

  val maxIdle = 10 seconds
  val url = "http://localhost:" + port
  val scalaVersionR = """^(\d\.\d+)\..+""".r
  val scalaVersionR(scalaVersion) = scala.util.Properties.versionNumberString

  private var server: Option[Server] = None

  def start(): Unit = synchronized {
    if (server.isEmpty) server = Some(newServer)
  }

  def shutdown(): Unit = synchronized {
    server foreach stopServer
    server = None
  }

  ThreadUtils.runOnShutdown("LiveTestUtils", shutdown())

  private def createTempDir(prefix: String, suffix: String = ""): File = {
    val tmpDir = File.createTempFile(prefix, suffix)
    tmpDir.delete
    tmpDir.mkdir
    FileUtils.forceDeleteOnExit(tmpDir)
    tmpDir
  }

  private def newServer: Server = {
    info("Starting Jetty")

    // Determine webapp project root dir
    def tryPrefix(prefix: String): Option[String] =
      if (new File(prefix + "src/main/webapp").exists) Some(prefix) else None
    val webappProjectRoot: String =
      tryPrefix("") orElse tryPrefix("webapp-server/") get
    def file(relPath: String) = new File(webappProjectRoot + relPath)

    // Manually create an exploded WAR
    // Could use sbt or a real WAR but then we can't easily/quickly run single tests from IDE
    val tmpWarDir = createTempDir("shipreq-server-test-war")
    FileUtils.copyDirectory(file("src/main/webapp"), tmpWarDir)

    // Copy a few assets over
    val assetDir = System.getProperty("shipreq.assets")
    def assetFile(s: String) = new File(s"$assetDir/$s")
    def warFile(s: String) = new File(s"${tmpWarDir.getAbsolutePath}/$s")
    def copyAsset(s: String) = FileUtils.copyFile(assetFile(s), new File(s"$tmpWarDir/$s"))
    copyAsset(AssetManifest.favicon)
    FileUtils.write(warFile(AssetManifest.webappClientPublicJs), "function public(){}") // Fake content
    FileUtils.write(warFile(AssetManifest.webappClientHomeJs), "function home(){}") // Fake content
    FileUtils.write(warFile(AssetManifest.webappClientProjectJs), "function project(){}") // Fake content
//    ;{ import sys.process._; s"ls -la $tmpWarDir".! };

    val svr = new Server
    val http = new ServerConnector(svr, new HttpConnectionFactory(new HttpConfiguration))
    http.setPort(port)
    http.setIdleTimeout(maxIdle.millis)
    svr.setConnectors(Array(http))

    val context = new WebAppContext
    context.setContextPath("/")
    context.setWar(tmpWarDir.getAbsolutePath)
    context.setThrowUnavailableOnStartupException(true)
    svr.setHandler(context)

    context.setServer(svr)
    svr.start()
    svr
  }

  private def stopServer(s: Server): Unit = {
    info("Stopping Jetty")
    s.stop()
    s.join()
  }
}
