package shipreq.base.ops

import java.io.{Closeable, InputStream, PrintWriter, Reader}
import java.net.URL
import java.sql.{Blob, CallableStatement, Clob, Connection, DatabaseMetaData, Date, NClob, ParameterMetaData, PreparedStatement, Ref, ResultSet, ResultSetMetaData, RowId, SQLWarning, SQLXML, Savepoint, Statement, Struct, Time, Timestamp}
import java.util.concurrent.Executor
import java.util.logging.Logger
import java.util.{Calendar, Properties}
import java.{sql, util}
import javax.sql.DataSource
import scala.util.control.NonFatal
import scalaz.Semigroup

trait SqlTracer { outer =>
  final def inject(ds: DataSource): DataSource =
    new SqlTracer.DataSourceProxy(ds)(this)

  final def execute[@specialized(Boolean, Int, Long) A](method: String,
                                                        sql: String,
                                                        batches: Int,
                                                        run: () => A): A = {
    val startTimeNs = System.nanoTime()
    try {
      val a = run()
      val endTimeNs = System.nanoTime()
      logExecute(method, sql, batches, None, startTimeNs, endTimeNs)
      a
    } catch {
      case NonFatal(t) =>
        val endTimeNs = System.nanoTime()
        logExecute(method, sql, batches, Some(t), startTimeNs, endTimeNs)
        throw t
    }
  }

  def logExecute(method: String, sql: String, batches: Int,
                 err: Option[Throwable], startTimeNs: Long, endTimeNs: Long): Unit

  final def compose(inner: SqlTracer): SqlTracer =
    new SqlTracer {
      override def logExecute(method: String, sql: String, batches: Int,
                              err: Option[Throwable], startTimeNs: Long, endTimeNs: Long): Unit = {
        outer.logExecute(method, sql, batches, err, startTimeNs, endTimeNs)
        inner.logExecute(method, sql, batches, err, startTimeNs, endTimeNs)
      }
    }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object SqlTracer {

  implicit val semigroup: Semigroup[SqlTracer] =
    _ compose _

  final class DataSourceProxy(underlying: DataSource)(implicit tracer: SqlTracer) extends DataSource with Closeable {
    def proxyC(c: Connection) = new ConnectionProxy(c)

    override def getConnection: Connection =
      proxyC(
        underlying.getConnection)

    override def getConnection(username: String, password: String): Connection =
      proxyC(
        underlying.getConnection(username, password))

    override def close(): Unit =
      underlying match {
        case c: Closeable => c.close()
        case _            => ()
      }

    override def setLoginTimeout(seconds: Int): Unit =
      underlying.setLoginTimeout(seconds)

    override def setLogWriter(out: PrintWriter): Unit =
      underlying.setLogWriter(out)

    override def getParentLogger: Logger =
      underlying.getParentLogger

    override def getLoginTimeout: Int =
      underlying.getLoginTimeout

    override def getLogWriter: PrintWriter =
      underlying.getLogWriter

    override def unwrap[T](iface: Class[T]): T =
      underlying.unwrap(iface)

    override def isWrapperFor(iface: Class[_]): Boolean =
      underlying.isWrapperFor(iface)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class ConnectionProxy(underlying: Connection)(implicit tracer: SqlTracer) extends Connection {
    def proxyPS(sql: String, ps: PreparedStatement) = new PreparedStatementProxy(sql, ps)

    override def abort(executor: Executor): Unit =
      underlying.abort(executor)

    override def clearWarnings(): Unit =
      underlying.clearWarnings()

    override def close(): Unit =
      underlying.close()

    override def commit(): Unit =
      underlying.commit()

    override def createArrayOf(typeName: String, elements: Array[AnyRef]): sql.Array =
      underlying.createArrayOf(typeName, elements)

    override def createBlob(): Blob =
      underlying.createBlob()

    override def createClob(): Clob =
      underlying.createClob()

    override def createNClob(): NClob =
      underlying.createNClob()

    override def createSQLXML(): SQLXML =
      underlying.createSQLXML()

    override def createStatement(): Statement =
      underlying.createStatement()

    override def createStatement(resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int): Statement =
      underlying.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability)

    override def createStatement(resultSetType: Int, resultSetConcurrency: Int): Statement =
      underlying.createStatement(resultSetType, resultSetConcurrency)

    override def createStruct(typeName: String, attributes: Array[AnyRef]): Struct =
      underlying.createStruct(typeName, attributes)

    override def getAutoCommit: Boolean =
      underlying.getAutoCommit

    override def getCatalog: String =
      underlying.getCatalog

    override def getClientInfo: Properties =
      underlying.getClientInfo

    override def getClientInfo(name: String): String =
      underlying.getClientInfo(name)

    override def getHoldability: Int =
      underlying.getHoldability

    override def getMetaData: DatabaseMetaData =
      underlying.getMetaData

    override def getNetworkTimeout: Int =
      underlying.getNetworkTimeout

    override def getSchema: String =
      underlying.getSchema

    override def getTransactionIsolation: Int =
      underlying.getTransactionIsolation

    override def getTypeMap: util.Map[String, Class[_]] =
      underlying.getTypeMap

    override def getWarnings: SQLWarning =
      underlying.getWarnings

    override def isClosed: Boolean =
      underlying.isClosed

    override def isReadOnly: Boolean =
      underlying.isReadOnly

    override def isValid(timeout: Int): Boolean =
      underlying.isValid(timeout)

    override def isWrapperFor(iface: Class[_]): Boolean =
      underlying.isWrapperFor(iface)

    override def nativeSQL(sql: String): String =
      underlying.nativeSQL(sql)

    override def prepareCall(sql: String, resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int): CallableStatement =
      underlying.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability)

    override def prepareCall(sql: String, resultSetType: Int, resultSetConcurrency: Int): CallableStatement =
      underlying.prepareCall(sql, resultSetType, resultSetConcurrency)

    override def prepareCall(sql: String): CallableStatement =
      underlying.prepareCall(sql)

    override def prepareStatement(sql: String, autoGeneratedKeys: Int): PreparedStatement =
      proxyPS(sql,
        underlying.prepareStatement(sql, autoGeneratedKeys))

    override def prepareStatement(sql: String, columnIndexes: Array[Int]): PreparedStatement =
      proxyPS(sql,
        underlying.prepareStatement(sql, columnIndexes))

    override def prepareStatement(sql: String, columnNames: Array[String]): PreparedStatement =
      proxyPS(sql,
        underlying.prepareStatement(sql, columnNames))

    override def prepareStatement(sql: String, resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int): PreparedStatement =
      proxyPS(sql,
        underlying.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability))

    override def prepareStatement(sql: String, resultSetType: Int, resultSetConcurrency: Int): PreparedStatement =
      proxyPS(sql,
        underlying.prepareStatement(sql, resultSetType, resultSetConcurrency))

    override def prepareStatement(sql: String): PreparedStatement =
      proxyPS(sql,
        underlying.prepareStatement(sql))

    override def releaseSavepoint(savepoint: Savepoint): Unit =
      underlying.releaseSavepoint(savepoint)

    override def rollback(): Unit =
      underlying.rollback()

    override def rollback(savepoint: Savepoint): Unit =
      underlying.rollback(savepoint)

    override def setAutoCommit(autoCommit: Boolean): Unit =
      underlying.setAutoCommit(autoCommit)

    override def setCatalog(catalog: String): Unit =
      underlying.setCatalog(catalog)

    override def setClientInfo(name: String, value: String): Unit =
      underlying.setClientInfo(name, value)

    override def setClientInfo(properties: Properties): Unit =
      underlying.setClientInfo(properties)

    override def setHoldability(holdability: Int): Unit =
      underlying.setHoldability(holdability)

    override def setNetworkTimeout(executor: Executor, milliseconds: Int): Unit =
      underlying.setNetworkTimeout(executor, milliseconds)

    override def setReadOnly(readOnly: Boolean): Unit =
      underlying.setReadOnly(readOnly)

    override def setSavepoint(): Savepoint =
      underlying.setSavepoint()

    override def setSavepoint(name: String): Savepoint =
      underlying.setSavepoint(name)

    override def setSchema(schema: String): Unit =
      underlying.setSchema(schema)

    override def setTransactionIsolation(level: Int): Unit =
      underlying.setTransactionIsolation(level)

    override def setTypeMap(map: util.Map[String, Class[_]]): Unit =
      underlying.setTypeMap(map)

    override def unwrap[T](iface: Class[T]): T =
      underlying.unwrap[T](iface)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class PreparedStatementProxy(preparedSql: String, underlying: PreparedStatement)(implicit tracer: SqlTracer) extends PreparedStatement {
    var batchSize = 0

    override def addBatch(): Unit = {
      batchSize += 1
      underlying.addBatch()
    }

    override def addBatch(sql: String): Unit =
      underlying.addBatch(sql: String) // Spec declares this a runtime exception

    override def cancel(): Unit =
      underlying.cancel()

    override def clearBatch(): Unit =
      underlying.clearBatch()

    override def clearParameters(): Unit =
      underlying.clearParameters()

    override def clearWarnings(): Unit =
      underlying.clearWarnings()

    override def close(): Unit =
      underlying.close()

    override def closeOnCompletion(): Unit =
      underlying.closeOnCompletion()

    override def execute(): Boolean =
      tracer.execute("execute", preparedSql, 1, () =>
        underlying.execute())

    override def execute(sql: String, autoGeneratedKeys: Int): Boolean =
      underlying.execute(sql: String, autoGeneratedKeys: Int) // Spec declares this a runtime exception

    override def execute(sql: String, columnIndexes: Array[Int]): Boolean =
      underlying.execute(sql: String, columnIndexes: Array[Int]) // Spec declares this a runtime exception

    override def execute(sql: String, columnNames: Array[String]): Boolean =
      underlying.execute(sql: String, columnNames: Array[String]) // Spec declares this a runtime exception

    override def execute(sql: String): Boolean =
      underlying.execute(sql: String) // Spec declares this a runtime exception

    override def executeBatch(): Array[Int] =
      tracer.execute("executeBatch", preparedSql, batchSize, () =>
        underlying.executeBatch())

    override def executeLargeBatch(): Array[Long] =
      tracer.execute("executeLargeBatch", preparedSql, batchSize, () =>
        underlying.executeLargeBatch())

    override def executeLargeUpdate(): Long =
      tracer.execute("executeLargeUpdate", preparedSql, 1, () =>
        underlying.executeLargeUpdate())

    override def executeQuery(): ResultSet =
      tracer.execute("executeQuery", preparedSql, 1, () =>
        underlying.executeQuery())

    override def executeQuery(sql: String): ResultSet =
      underlying.executeQuery(sql) // Spec declares this a runtime exception

    override def executeUpdate(): Int =
      tracer.execute("executeUpdate", preparedSql, 1, () =>
        underlying.executeUpdate())

    override def executeUpdate(sql: String, autoGeneratedKeys: Int): Int =
      underlying.executeUpdate(sql: String, autoGeneratedKeys: Int) // Spec declares this a runtime exception

    override def executeUpdate(sql: String, columnIndexes: Array[Int]): Int =
      underlying.executeUpdate(sql: String, columnIndexes: Array[Int]) // Spec declares this a runtime exception

    override def executeUpdate(sql: String, columnNames: Array[String]): Int =
      underlying.executeUpdate(sql: String, columnNames: Array[String]) // Spec declares this a runtime exception

    override def executeUpdate(sql: String): Int =
      underlying.executeUpdate(sql: String) // Spec declares this a runtime exception

    override def getConnection: Connection =
      underlying.getConnection

    override def getFetchDirection: Int =
      underlying.getFetchDirection

    override def getFetchSize: Int =
      underlying.getFetchSize

    override def getGeneratedKeys: ResultSet =
      underlying.getGeneratedKeys

    override def getMaxFieldSize: Int =
      underlying.getMaxFieldSize

    override def getMaxRows: Int =
      underlying.getMaxRows

    override def getMetaData: ResultSetMetaData =
      underlying.getMetaData

    override def getMoreResults: Boolean =
      underlying.getMoreResults

    override def getMoreResults(current: Int): Boolean =
      underlying.getMoreResults(current: Int)

    override def getParameterMetaData: ParameterMetaData =
      underlying.getParameterMetaData

    override def getQueryTimeout: Int =
      underlying.getQueryTimeout

    override def getResultSet: ResultSet =
      underlying.getResultSet

    override def getResultSetConcurrency: Int =
      underlying.getResultSetConcurrency

    override def getResultSetHoldability: Int =
      underlying.getResultSetHoldability

    override def getResultSetType: Int =
      underlying.getResultSetType

    override def getUpdateCount: Int =
      underlying.getUpdateCount

    override def getWarnings: SQLWarning =
      underlying.getWarnings

    override def isClosed: Boolean =
      underlying.isClosed

    override def isCloseOnCompletion: Boolean =
      underlying.isCloseOnCompletion

    override def isPoolable: Boolean =
      underlying.isPoolable

    override def isWrapperFor(iface: Class[_]): Boolean =
      underlying.isWrapperFor(iface: Class[_])

    override def setArray(parameterIndex: Int, x: sql.Array): Unit =
      underlying.setArray(parameterIndex: Int, x: sql.Array)

    override def setAsciiStream(parameterIndex: Int, x: InputStream, length: Int): Unit =
      underlying.setAsciiStream(parameterIndex: Int, x: InputStream, length: Int)

    override def setAsciiStream(parameterIndex: Int, x: InputStream, length: Long): Unit =
      underlying.setAsciiStream(parameterIndex: Int, x: InputStream, length: Long)

    override def setAsciiStream(parameterIndex: Int, x: InputStream): Unit =
      underlying.setAsciiStream(parameterIndex: Int, x: InputStream)

    override def setBigDecimal(parameterIndex: Int, x: java.math.BigDecimal): Unit =
      underlying.setBigDecimal(parameterIndex: Int, x: java.math.BigDecimal)

    override def setBinaryStream(parameterIndex: Int, x: InputStream, length: Int): Unit =
      underlying.setBinaryStream(parameterIndex: Int, x: InputStream, length: Int)

    override def setBinaryStream(parameterIndex: Int, x: InputStream, length: Long): Unit =
      underlying.setBinaryStream(parameterIndex: Int, x: InputStream, length: Long)

    override def setBinaryStream(parameterIndex: Int, x: InputStream): Unit =
      underlying.setBinaryStream(parameterIndex: Int, x: InputStream)

    override def setBlob(parameterIndex: Int, inputStream: InputStream, length: Long): Unit =
      underlying.setBlob(parameterIndex: Int, inputStream: InputStream, length: Long)

    override def setBlob(parameterIndex: Int, inputStream: InputStream): Unit =
      underlying.setBlob(parameterIndex: Int, inputStream: InputStream)

    override def setBlob(parameterIndex: Int, x: Blob): Unit =
      underlying.setBlob(parameterIndex: Int, x: Blob)

    override def setBoolean(parameterIndex: Int, x: Boolean): Unit =
      underlying.setBoolean(parameterIndex: Int, x: Boolean)

    override def setByte(parameterIndex: Int, x: Byte): Unit =
      underlying.setByte(parameterIndex: Int, x: Byte)

    override def setBytes(parameterIndex: Int, x: Array[Byte]): Unit =
      underlying.setBytes(parameterIndex: Int, x: Array[Byte])

    override def setCharacterStream(parameterIndex: Int, reader: Reader, length: Int): Unit =
      underlying.setCharacterStream(parameterIndex: Int, reader: Reader, length: Int)

    override def setCharacterStream(parameterIndex: Int, reader: Reader, length: Long): Unit =
      underlying.setCharacterStream(parameterIndex: Int, reader: Reader, length: Long)

    override def setCharacterStream(parameterIndex: Int, reader: Reader): Unit =
      underlying.setCharacterStream(parameterIndex: Int, reader: Reader)

    override def setClob(parameterIndex: Int, reader: Reader, length: Long): Unit =
      underlying.setClob(parameterIndex: Int, reader: Reader, length: Long)

    override def setClob(parameterIndex: Int, reader: Reader): Unit =
      underlying.setClob(parameterIndex: Int, reader: Reader)

    override def setClob(parameterIndex: Int, x: Clob): Unit =
      underlying.setClob(parameterIndex: Int, x: Clob)

    override def setCursorName(name: String): Unit =
      underlying.setCursorName(name: String)

    override def setDate(parameterIndex: Int, x: Date, cal: Calendar): Unit =
      underlying.setDate(parameterIndex: Int, x: Date, cal: Calendar)

    override def setDate(parameterIndex: Int, x: Date): Unit =
      underlying.setDate(parameterIndex: Int, x: Date)

    override def setDouble(parameterIndex: Int, x: Double): Unit =
      underlying.setDouble(parameterIndex: Int, x: Double)

    override def setEscapeProcessing(enable: Boolean): Unit =
      underlying.setEscapeProcessing(enable: Boolean)

    override def setFetchDirection(direction: Int): Unit =
      underlying.setFetchDirection(direction: Int)

    override def setFetchSize(rows: Int): Unit =
      underlying.setFetchSize(rows: Int)

    override def setFloat(parameterIndex: Int, x: Float): Unit =
      underlying.setFloat(parameterIndex: Int, x: Float)

    override def setInt(parameterIndex: Int, x: Int): Unit =
      underlying.setInt(parameterIndex: Int, x: Int)

    override def setLong(parameterIndex: Int, x: Long): Unit =
      underlying.setLong(parameterIndex: Int, x: Long)

    override def setMaxFieldSize(max: Int): Unit =
      underlying.setMaxFieldSize(max: Int)

    override def setMaxRows(max: Int): Unit =
      underlying.setMaxRows(max: Int)

    override def setNCharacterStream(parameterIndex: Int, value: Reader, length: Long): Unit =
      underlying.setNCharacterStream(parameterIndex: Int, value: Reader, length: Long)

    override def setNCharacterStream(parameterIndex: Int, value: Reader): Unit =
      underlying.setNCharacterStream(parameterIndex: Int, value: Reader)

    override def setNClob(parameterIndex: Int, reader: Reader, length: Long): Unit =
      underlying.setNClob(parameterIndex: Int, reader: Reader, length: Long)

    override def setNClob(parameterIndex: Int, reader: Reader): Unit =
      underlying.setNClob(parameterIndex: Int, reader: Reader)

    override def setNClob(parameterIndex: Int, value: NClob): Unit =
      underlying.setNClob(parameterIndex: Int, value: NClob)

    override def setNString(parameterIndex: Int, value: String): Unit =
      underlying.setNString(parameterIndex: Int, value: String)

    override def setNull(parameterIndex: Int, sqlType: Int, typeName: String): Unit =
      underlying.setNull(parameterIndex: Int, sqlType: Int, typeName: String)

    override def setNull(parameterIndex: Int, sqlType: Int): Unit =
      underlying.setNull(parameterIndex: Int, sqlType: Int)

    override def setObject(parameterIndex: Int, x: Any, targetSqlType: Int, scaleOrLength: Int): Unit =
      underlying.setObject(parameterIndex: Int, x: Any, targetSqlType: Int, scaleOrLength: Int)

    override def setObject(parameterIndex: Int, x: Any, targetSqlType: Int): Unit =
      underlying.setObject(parameterIndex: Int, x: Any, targetSqlType: Int)

    override def setObject(parameterIndex: Int, x: Any): Unit =
      underlying.setObject(parameterIndex: Int, x: Any)

    override def setPoolable(poolable: Boolean): Unit =
      underlying.setPoolable(poolable: Boolean)

    override def setQueryTimeout(seconds: Int): Unit =
      underlying.setQueryTimeout(seconds: Int)

    override def setRef(parameterIndex: Int, x: Ref): Unit =
      underlying.setRef(parameterIndex: Int, x: Ref)

    override def setRowId(parameterIndex: Int, x: RowId): Unit =
      underlying.setRowId(parameterIndex: Int, x: RowId)

    override def setShort(parameterIndex: Int, x: Short): Unit =
      underlying.setShort(parameterIndex: Int, x: Short)

    override def setSQLXML(parameterIndex: Int, xmlObject: SQLXML): Unit =
      underlying.setSQLXML(parameterIndex: Int, xmlObject: SQLXML)

    override def setString(parameterIndex: Int, x: String): Unit =
      underlying.setString(parameterIndex: Int, x: String)

    override def setTime(parameterIndex: Int, x: Time, cal: Calendar): Unit =
      underlying.setTime(parameterIndex: Int, x: Time, cal: Calendar)

    override def setTime(parameterIndex: Int, x: Time): Unit =
      underlying.setTime(parameterIndex: Int, x: Time)

    override def setTimestamp(parameterIndex: Int, x: Timestamp, cal: Calendar): Unit =
      underlying.setTimestamp(parameterIndex: Int, x: Timestamp, cal: Calendar)

    override def setTimestamp(parameterIndex: Int, x: Timestamp): Unit =
      underlying.setTimestamp(parameterIndex: Int, x: Timestamp)

    @deprecated("", "")
    override def setUnicodeStream(parameterIndex: Int, x: InputStream, length: Int): Unit =
      underlying.setUnicodeStream(parameterIndex: Int, x: InputStream, length: Int)

    override def setURL(parameterIndex: Int, x: URL): Unit =
      underlying.setURL(parameterIndex: Int, x: URL)

    override def unwrap[T](iface: Class[T]): T =
      underlying.unwrap[T](iface: Class[T])
  }
}
