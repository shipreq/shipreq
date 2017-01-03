package shipreq.base.util.log

import japgolly.microlibs.config.ConfigParser
import java.util.Locale
import org.slf4j.{LoggerFactory, Logger => slf4jLogger}
import scalaz.Applicative
import shipreq.base.util.Error

object Logger {
  def forClass(c: Class[_]): Logger =
    new Logger(LoggerFactory.getLogger(loggingName(c)))

  def loggingName(c: Class[_]): String =
    if (c.getCanonicalName ne null)
      c.getCanonicalName.replaceFirst("\\$$", "")
    else
      c.toString.replaceFirst("^Class ", "")
}

trait HasLogger {
  final protected val log = Logger.forClass(getClass)
}

sealed trait LogLevel
object LogLevel {
  case object Trace extends LogLevel
  case object Debug extends LogLevel
  case object Info  extends LogLevel
  case object Warn  extends LogLevel
  case object Error extends LogLevel
  case object Off   extends LogLevel
  val values = List(Off, Trace, Debug, Info, Warn, Error)

  def read(s: String): Option[LogLevel] =
    s.trim.toLowerCase(Locale.ENGLISH) match {
      case "off"   => Some(Off)
      case "trace" => Some(Trace)
      case "debug" => Some(Debug)
      case "info"  => Some(Info)
      case "warn"  => Some(Warn)
      case "error" => Some(Error)
      case _       => None
    }

  implicit def configParserShipReqLogLevel(implicit p: ConfigParser[String]): ConfigParser[LogLevel] =
    p.mapOption(read)
}

final class Logger(log: slf4jLogger) {

  sealed abstract class AtLevel {
    def ? : Boolean
    def apply(msg: String): Unit
    def apply(t: Throwable, msg: String): Unit
    def apply(fmt: String, arg: AnyRef): Unit
    def apply(fmt: String, arg1: AnyRef, arg2: AnyRef): Unit
    def applyN(fmt: String, args: AnyRef*): Unit

    @inline final def z(              msg: => String): Unit = if (?) apply(msg)
    @inline final def z(t: Throwable, msg: => String): Unit = if (?) apply(t, msg)

    @inline private[this] def f1(f: String, a: Any        ): String = f.format(a)
    @inline private[this] def f2(f: String, a: Any, b: Any): String = f.format(a, b)
    @inline private[this] def fn(f: String, as: Seq[Any]  ): String = f.format(as: _*)

    @inline final def fmt (              f: String, arg : Any           ): Unit = if (?) apply(f1(f, arg))
    @inline final def fmt (              f: String, arg1: Any, arg2: Any): Unit = if (?) apply(f2(f, arg1, arg2: Any))
    @inline final def fmtN(              f: String, args: Any*          ): Unit = if (?) apply(fn(f, args))
    @inline final def fmt (t: Throwable, f: String, arg : Any           ): Unit = if (?) apply(t, f1(f, arg))
    @inline final def fmt (t: Throwable, f: String, arg1: Any, arg2: Any): Unit = if (?) apply(t, f2(f, arg1, arg2: Any))
    @inline final def fmtN(t: Throwable, f: String, args: Any*          ): Unit = if (?) apply(t, fn(f, args))

    @inline final def apply(e: Error, msg: String)                   : Unit = apply(e.throwable, msg)
    @inline final def z(e: Error, msg: => String)                    : Unit = z(e.throwable, msg)
    @inline final def fmt (e: Error, f: String, arg : Any           ): Unit = fmt(e.throwable, f, arg)
    @inline final def fmt (e: Error, f: String, arg1: Any, arg2: Any): Unit = fmt(e.throwable, f, arg1, arg2)
    @inline final def fmtN(e: Error, f: String, args: Any*          ): Unit = fmtN(e.throwable, f, args: _*)

    final def printer[M[_]](implicit M: Applicative[M]): (=> String) => M[Unit] =
      if (?)
        msg => M.point(z(msg))
      else {
        val u = M.point(())
        _ => u
      }
  }

  object off extends AtLevel {
    @inline override def ?                                             : Boolean = false
    @inline override def apply(msg: String)                            : Unit    = ()
    @inline override def apply(t: Throwable, msg: String)              : Unit    = ()
    @inline override def apply(fmt: String, arg: AnyRef)               : Unit    = ()
    @inline override def apply(fmt: String, arg1: AnyRef, arg2: AnyRef): Unit    = ()
    @inline override def applyN(fmt: String, args: AnyRef*)            : Unit    = ()
  }

  object trace extends AtLevel {
    @inline override def ?                                             : Boolean = log.isTraceEnabled
    @inline override def apply(msg: String)                            : Unit    = log.trace(msg)
    @inline override def apply(t: Throwable, msg: String)              : Unit    = log.trace(msg, t)
    @inline override def apply(fmt: String, arg: AnyRef)               : Unit    = log.trace(fmt, arg)
    @inline override def apply(fmt: String, arg1: AnyRef, arg2: AnyRef): Unit    = log.trace(fmt, arg1, arg2: Any)
    @inline override def applyN(fmt: String, args: AnyRef*)            : Unit    = log.trace(fmt, args: _*)
  }

  object debug extends AtLevel {
    @inline override def ?                                             : Boolean = log.isDebugEnabled
    @inline override def apply(msg: String)                            : Unit    = log.debug(msg)
    @inline override def apply(t: Throwable, msg: String)              : Unit    = log.debug(msg, t)
    @inline override def apply(fmt: String, arg: AnyRef)               : Unit    = log.debug(fmt, arg)
    @inline override def apply(fmt: String, arg1: AnyRef, arg2: AnyRef): Unit    = log.debug(fmt, arg1, arg2: Any)
    @inline override def applyN(fmt: String, args: AnyRef*)            : Unit    = log.debug(fmt, args: _*)
  }

  object info extends AtLevel {
    @inline override def ?                                             : Boolean = log.isInfoEnabled
    @inline override def apply(msg: String)                            : Unit    = log.info(msg)
    @inline override def apply(t: Throwable, msg: String)              : Unit    = log.info(msg, t)
    @inline override def apply(fmt: String, arg: AnyRef)               : Unit    = log.info(fmt, arg)
    @inline override def apply(fmt: String, arg1: AnyRef, arg2: AnyRef): Unit    = log.info(fmt, arg1, arg2: Any)
    @inline override def applyN(fmt: String, args: AnyRef*)            : Unit    = log.info(fmt, args: _*)
  }

  object warn extends AtLevel {
    @inline override def ?                                             : Boolean = log.isWarnEnabled
    @inline override def apply(msg: String)                            : Unit    = log.warn(msg)
    @inline override def apply(t: Throwable, msg: String)              : Unit    = log.warn(msg, t)
    @inline override def apply(fmt: String, arg: AnyRef)               : Unit    = log.warn(fmt, arg)
    @inline override def apply(fmt: String, arg1: AnyRef, arg2: AnyRef): Unit    = log.warn(fmt, arg1, arg2: Any)
    @inline override def applyN(fmt: String, args: AnyRef*)            : Unit    = log.warn(fmt, args: _*)
  }

  object error extends AtLevel {
    @inline override def ?                                             : Boolean = log.isErrorEnabled
    @inline override def apply(msg: String)                            : Unit    = log.error(msg)
    @inline override def apply(t: Throwable, msg: String)              : Unit    = log.error(msg, t)
    @inline override def apply(fmt: String, arg: AnyRef)               : Unit    = log.error(fmt, arg)
    @inline override def apply(fmt: String, arg1: AnyRef, arg2: AnyRef): Unit    = log.error(fmt, arg1, arg2: Any)
    @inline override def applyN(fmt: String, args: AnyRef*)            : Unit    = log.error(fmt, args: _*)
  }
  
  @inline def atLevel(l: LogLevel): AtLevel = l match {
    case LogLevel.Debug => debug
    case LogLevel.Info  => info
    case LogLevel.Warn  => warn
    case LogLevel.Error => error
    case LogLevel.Trace => trace
    case LogLevel.Off   => off
  }

  @inline def mdc = MDC
}
