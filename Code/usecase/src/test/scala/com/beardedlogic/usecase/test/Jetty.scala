package com.beardedlogic.usecase.test

import net.liftweb.util.TimeHelpers._
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.webapp.WebAppContext

/**
 * Starts up an instance of Jetty than runs the webapp.
 *
 * @since 29/04/2013
 */
object Jetty {

  private val jetty = SharedGlobal(Some(1000L), newServer _) { s => s.stop; s.join }

  def acquire(): Server = jetty.acquire

  def release(): Server = jetty.release

  val PORT = 8090
  val MAX_IDLE = 10 seconds
  val URL = "http://localhost:" + PORT

  def newServer: Server = {
    val svr = new Server

    val connector = new SelectChannelConnector
    connector.setPort(PORT)
    connector.setMaxIdleTime(MAX_IDLE.millis.toInt)
    connector.setServer(svr)
    svr.setConnectors(Array(connector));

    val context = new WebAppContext
    context.setContextPath("/")
    context.setWar("src/main/webapp")
    // context.setClassLoader(Thread.currentThread().getContextClassLoader());
    // context.setDescriptor("src/main/webapp/WEB-INF/web.xml")
    // context.setResourceBase("src/main/webapp")
    svr.setHandler(context)

    context.setServer(svr)
    svr.start
    svr
  }
}