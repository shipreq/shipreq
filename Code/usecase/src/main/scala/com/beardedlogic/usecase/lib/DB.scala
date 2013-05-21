package com.beardedlogic.usecase.lib

import net.liftweb.util.Props
import org.postgresql.ds.PGSimpleDataSource
import scala.slick.session.Database
import ch.qos.logback.classic.Level
import net.liftweb.common.Logger

/**
 * Database connectivity.
 *
 * @since 21/05/2013
 */
object DB extends Logger {

  @inline private def n(name: String) = s"db.$name"
  @inline private def prop(name: String) = Props.get(n(name)) ?~ s"Property not found: ${n(name)}"
  @inline private def setIfDefinedS(name: String, setFn: String => Unit) = Props.get(n(name)).foreach(setFn(_))
  @inline private def setIfDefinedI(name: String, setFn: Int => Unit) = Props.getInt(n(name)).foreach(setFn(_))
  @inline private def setIfDefinedB(name: String, setFn: Boolean => Unit) = Props.getBool(n(name)).foreach(setFn(_))

  private def loadDataSource() = for {
    database <- prop("database")
    username <- prop("username")
    password <- prop("password")
  } yield {
    val ds = new PGSimpleDataSource
    ds.setDatabaseName(database)
    ds.setUser(username)
    ds.setPassword(password)
    setIfDefinedS("appname", ds.setApplicationName(_))
    setIfDefinedB("binary_transfer", ds.setBinaryTransfer(_))
    setIfDefinedS("binary_transfer_disable", ds.setBinaryTransferDisable(_))
    setIfDefinedS("binary_transfer_enable", ds.setBinaryTransferEnable(_))
    setIfDefinedS("compatible", ds.setCompatible(_))
    setIfDefinedI("login_timeout", ds.setLoginTimeout(_))
    setIfDefinedI("port", ds.setPortNumber(_))
    setIfDefinedI("prepare_threshold", ds.setPrepareThreshold(_))
    setIfDefinedI("protocol_ver", ds.setProtocolVersion(_))
    setIfDefinedI("recv_buffer_size", ds.setReceiveBufferSize(_))
    setIfDefinedI("send_buffer_size", ds.setSendBufferSize(_))
    setIfDefinedS("host", ds.setServerName(_))
    setIfDefinedI("socket_timeout", ds.setSocketTimeout(_))
    setIfDefinedB("ssl", ds.setSsl(_))
    setIfDefinedS("ssl_factory", ds.setSslfactory(_))
    setIfDefinedB("tcp_keep_alive", ds.setTcpKeepAlive(_))
    setIfDefinedI("unknown_length", ds.setUnknownLength(_))
    prop("log_level").foreach(l => ds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))
    ds
  }

  private def newDatabase() = {
    val ds = loadDataSource().openOrThrowException("A database connection is mandatory.")
    debug("Connecting to database: " + ds.getDatabaseName)
    ds.getConnection.close() // test the data source validity
    Database.forDataSource(ds)
  }

  val Slick = newDatabase()

  def init() {} // Nothing to do. Connection is val.
}
