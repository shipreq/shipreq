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

  val PORT = 8090
  val MAX_IDLE = 10 seconds
  val URL = "http://localhost:" + PORT + "/"

  private val server: Server = {
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
    svr
  }

  def start() {
    //    if (!server.isStarted) {
    server.start
    //    server.setStopAtShutdown(true)
    //      Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
    //        override def run() {
    //          println("Here")
    //          if (server.isStarted()) {
    //            server.stop
    //            server.join
    //          }
    //        }
    //      }, "Stop Jetty Hook"));
    //    }
  }

  def stop() {
    server.stop
    server.join
  }
}