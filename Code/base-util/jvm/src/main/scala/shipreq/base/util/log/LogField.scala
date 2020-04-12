package shipreq.base.util.log

import com.fasterxml.jackson.core.JsonGenerator
import io.circe.{Encoder, Json}
import io.circe.syntax._
import japgolly.univeq._
import java.time.Duration
import java.util.UUID
import net.logstash.logback.argument.StructuredArgument
import org.slf4j.MDC

sealed abstract class LogField[T <: LogField.Type, @specialized(Long, Boolean) -A] private[log] (final val key: String) { self =>

  val fieldType: T
  protected def conv: A => fieldType.Input

  final def apply(a: A): StructuredArgument =
    fieldType.create(key, conv(a))

  final def contramap[B](f: B => A): LogField[T, B] =
    new LogField[T, B](key) {
      override val fieldType: self.fieldType.type = self.fieldType
      override protected def conv: B => fieldType.Input = self.conv compose f
    }

  final def optional(o: Option[A]): StructuredArgument =
    o.fold(LogField.emptyArg)(apply)

  private final def convInput[T2 <: LogField.Type](t2: T2, i: fieldType.Input)(implicit ev: T =:= T2): t2.Input = {
    val _ = ev
    i.asInstanceOf[t2.Input]
  }

  final def mdcUnsafePut(value: A)(implicit ev: T =:= LogField.Text.type): Unit = {
    val str = convInput(LogField.Text, conv(value))
    MDC.put(key, str)
  }

  final def mdc(value: A)(implicit ev: T =:= LogField.Text.type): MdcValues = {
    val str = convInput(LogField.Text, conv(value))
    MdcValues.one(key, str)
  }
}

object LogField {

  protected[LogField] def apply(key: String, t: Type): LogField[t.type, t.Input] =
    generic[t.type](key, t)

  protected[LogField] def generic[T <: Type](key: String, t: T): LogField[T, t.Input] = {
    LogField.Registry.register(key, t)
    new LogField[T, t.Input](key) {
      override val fieldType: t.type = t
      override protected def conv: t.Input => t.Input = i => i
    }
  }

  sealed trait Type {
    type Input
    val create: (String, Input) => StructuredArgument
  }

  private def creator[A](write: (JsonGenerator, A) => Unit): (String, A) => StructuredArgument =
    (key, value) =>
      new StructuredArgument {
        override def writeTo(g: JsonGenerator) = {
          g.writeFieldName(key)
          write(g, value)
        }
        override def toString = "" + value
    }

  case object Text extends Type {
    override type Input = String
    override val create = creator[Input](_.writeString(_))

    def apply(key: String): LogField[Text.type, String] =
      LogField(key, this)

    def json[A: Encoder](key: String): LogField[Text.type, A] =
      apply(key).contramap(_.asJson.noSpaces)

    /** Useful because MDC accepts Strings only */
    def long(key: String): LogField[Text.type, Long] =
      apply(key).contramap(_.toString)

    def uuid(key: String): LogField[Text.type, UUID] =
      apply(key).contramap(_.toString)
  }

  case object Long extends Type {
    override type Input = Long
    override val create = creator[Input](_.writeNumber(_))

    def apply(key: String): LogField[Long.type, Long] =
      LogField(key, this)

    def durationMs(key: String): LogField[Long.type, Duration] =
      Long(key).contramap { d =>
        try
          d.toMillis
        catch {
          case _: ArithmeticException => scala.Long.MaxValue
        }
      }
  }

  case object Bool extends Type {
    override type Input = Boolean
    override val create = creator[Input](_.writeBoolean(_))

    def apply(key: String): LogField[Bool.type, Boolean] =
      LogField(key, this)
  }

  /** Unsafe because once ElasticSearch sees a certain type at a certain path, it tries to hold all future data to the
    * same expectation. This is safe so long as the JSON encoder always generates the same types at the same paths.
    */
  final case class UnsafeJson[A](encoder: Encoder[A]) extends Type {
    override type Input = A
    override val create = creator[Input]((g, a) => g.writeRawValue(a.asJson(encoder).noSpaces))

    override def equals(obj: Any) = obj match {
      case a: UnsafeJson[_] => encoder eq a.encoder
      case _                => false
    }
  }

  type UnsafeJsonLogField[A] = LogField[UnsafeJson[A], A]

  object UnsafeJson {

    def apply[A: Encoder](key: String): UnsafeJsonLogField[A] = {
      val fieldType = new UnsafeJson(Encoder[A])
      generic[UnsafeJson[A]](key, fieldType)
    }

    private val stringMapEncoder: Encoder[Iterable[(String, String)]] =
      Encoder.instance(i => Json.obj(i.iterator.map(x => x._1 -> Json.fromString(x._2)).toList: _*))

    def stringMap(key: String): UnsafeJsonLogField[Iterable[(String, String)]] =
      UnsafeJson(key)(stringMapEncoder)
  }

  // ===================================================================================================================

  implicit def univEqUnsafeJson_ : UnivEq[UnsafeJson[_]] = UnivEq.force
  implicit def univEqType        : UnivEq[Type]          = UnivEq.derive

  private[LogField] object Registry {
    private val lock  = new AnyRef
    private var state = Map.empty[String, Type]

    def register(k: String, t: Type): Unit =
      lock.synchronized {
        state.get(k) match {
          case Some(t2) =>
            if (t !=* t2)
              throw new ExceptionInInitializerError(s"Attempted to use log field '$k' with varying data types: $t & $t2")
          case None =>
            state = state.updated(k, t)
        }
      }
  }

  // ===================================================================================================================

  val emptyArg: StructuredArgument =
    new StructuredArgument {
      override def writeTo(g: JsonGenerator) = ()
      override def toString                  = ""
    }

}
