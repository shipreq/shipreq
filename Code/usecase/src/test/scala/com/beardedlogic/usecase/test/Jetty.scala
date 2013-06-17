package com.beardedlogic.usecase.test

import net.liftweb.common.Logger
import net.liftweb.util.TimeHelpers._
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.webapp.WebAppContext
import com.beardedlogic.usecase.lib.Misc
import java.io.File
import org.apache.commons.io.FileUtils

/**
 * Starts up an instance of Jetty than runs the webapp.
 *
 * @since 29/04/2013
 */
object Jetty extends Logger {

  Misc.ensureTestModeDuringTests()

  private val jetty = SharedGlobal(Some(15000L), newServer _)(stopServer(_))

  def acquire(): Server = jetty.acquire

  def release(): Server = jetty.release

  val PORT = 8090
  val MAX_IDLE = 10 seconds
  val URL = "http://localhost:" + PORT

  def newServer: Server = {
    // Manually create an exploded WAR
    // Could use sbt or a real WAR but then we can't easily/quickly run single tests from IDE
    val tmpWarDir = TestHelpers.createTempDir("usecase-test-war")
    FileUtils.copyDirectory(new File("src/main/webapp"), tmpWarDir)
    FileUtils.copyDirectory(new File("target/scala-2.10/resource_managed/main"), tmpWarDir)

    info("Starting Jetty")
    val svr = new Server

    val connector = new SelectChannelConnector
    connector.setPort(PORT)
    connector.setMaxIdleTime(MAX_IDLE.millis.toInt)
    connector.setServer(svr)
    svr.setConnectors(Array(connector))

    val context = new WebAppContext
    context.setContextPath("/")
    context.setWar(tmpWarDir.getAbsolutePath)
    svr.setHandler(context)

    context.setServer(svr)
    svr.start
    svr
  }

  def stopServer(s: Server) {
    info("Stopping Jetty")
    s.stop; s.join
  }
}
