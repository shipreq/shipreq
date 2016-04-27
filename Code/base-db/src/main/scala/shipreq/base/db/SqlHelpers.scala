package shipreq.base.db

import org.postgresql.util.PGobject
import scala.slick.jdbc.{GetResult, PositionedParameters, PositionedResult, SetParameter}
import shipreq.base.util.TaggedTypes._

object SqlHelpers {

  def pgObject(typ: String, value: String): PGobject = {
    val o = new PGobject()
    o.setType(typ)
    o.setValue(value)
    o
  }

  /** @param c [0..255] */
  def pgChar(c: Char): PGobject = {
    val o = new PGobject()
    o.setType("char")
    o.setValue(c.toString)
    o
  }

  implicit class GetResultObjExt(val o: GetResult.type) extends AnyVal {
    def const[A](a: A): GetResult[A] =
      GetResult(_ => a)
  }

  implicit class SetParameterExt[A](val sp: SetParameter[A]) extends AnyVal {
    def contramap[Z](f: Z => A): SetParameter[Z] =
      SetParameter[Z]((v, pp) => sp(f(v), pp))
  }

  implicit class PositionedParametersExt(private val pp: PositionedParameters) extends AnyVal {
    def setPgChar(c: Char): Unit =
      pp.setObject(pgChar(c), java.sql.Types.OTHER)
  }

  implicit class PositionedResultExt(private val r: PositionedResult) extends AnyVal {
    def nextPgChar(): Char =
      r.nextString().head
  }

  val SetNothing = SetParameter[Any]((_, _) => ())

  type DbCodec[A] = shipreq.base.db.DbCodecT[A, A]
  @inline val DbCodec = shipreq.base.db.DbCodecT

  @inline implicit def dbCodecToGet [A](implicit c: DbCodecT           [A, _]): GetResult   [A]         = c.get
  @inline implicit def dbCodecToSet [A](implicit c: DbCodecT           [_, A]): SetParameter[A]         = c.set
  @inline implicit def dbCodecOToGet[A](implicit c: DbCodec.WithOptionT[A, _]): GetResult   [Option[A]] = c.option.get
  @inline implicit def dbCodecOToSet[A](implicit c: DbCodec.WithOptionT[_, A]): SetParameter[Option[A]] = c.option.set

  implicit val dbCodecLong   = DbCodec.WithOption.summon[Long]
  implicit val dbCodecInt    = DbCodec.WithOption.summon[Int]
  implicit val dbCodecShort  = DbCodec.WithOption.summon[Short]
  implicit val dbCodecString = DbCodec.WithOption.summon[String]

  def SP_TaggedLongL[T <: TaggedType](implicit ev: T#U =:= Long): SetParameter[List[T]] =
    new SetParameter[List[T]] {
      override def apply(v: List[T], pp: PositionedParameters): Unit = {
        val sb = new StringBuilder
        sb append '{'
        if (v.nonEmpty) {
          sb append ev(v.head.value)
          v.tail.foreach(t => {
            sb append ','
            sb append ev(t.value)
          })
        }
        sb append '}'
        val o = pgObject("_int8", sb.toString)
        pp.setObject(o, java.sql.Types.OTHER)
    }
  }

  private[this] val SqlComments = """\s+--[^\r\n]*""".r
  private[this] val LeadingWhitespace = """[\r\n]+\s*""".r

  implicit class SqlStringExt(val s: String) extends AnyVal {
    def sql = {
      var t = s
      t = SqlComments.replaceAllIn(t, "")
      t = LeadingWhitespace.replaceAllIn(t, " ")
      t.trim
    }

    def inTable(table: String) = {
      val p = table + "."
      """(^|,)\s*""".r.replaceAllIn(s, _.group(0)+p)
    }
  }
}
