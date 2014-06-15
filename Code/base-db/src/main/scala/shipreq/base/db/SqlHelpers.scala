package shipreq.base.db

import org.postgresql.util.PGobject
import scala.slick.jdbc.{GetResult, SetParameter, PositionedResult, PositionedParameters}
import shipreq.base.util.TaggedTypes._

object SqlHelpers {

  @inline def pgObject(typ: String, value: String): PGobject = {
    val o = new PGobject()
    o.setType(typ)
    o.setValue(value)
    o
  }

  final case class ContramapSP[Z, A](f: Z => A, sp: SetParameter[A]) extends SetParameter[Z] {
    def apply(v: Z, pp: PositionedParameters): Unit = sp(f(v), pp)
  }

  implicit class SetParameterExt[A](val sp: SetParameter[A]) extends AnyVal {
    def contramap[Z](f: Z => A): SetParameter[Z] = ContramapSP(f, sp)
  }

  trait SqlForType[T] {
    def next(r: PositionedResult): T
    def nextO(r: PositionedResult): Option[T]
    def set(p: PositionedParameters, v: T): Unit
    def setO(p: PositionedParameters, v: Option[T]): Unit
  }

  implicit object SqlForTypeLong extends SqlForType[Long] {
    override def next(r: PositionedResult): Long = r.nextLong()
    override def nextO(r: PositionedResult): Option[Long] = r.nextLongOption()
    override def set(p: PositionedParameters, v: Long): Unit = p.setLong(v)
    override def setO(p: PositionedParameters, v: Option[Long]): Unit = p.setLongOption(v)
  }

  implicit object SqlForTypeShort extends SqlForType[Short] {
    override def next(r: PositionedResult): Short = r.nextShort()
    override def nextO(r: PositionedResult): Option[Short] = r.nextShortOption()
    override def set(p: PositionedParameters, v: Short): Unit = p.setShort(v)
    override def setO(p: PositionedParameters, v: Option[Short]): Unit = p.setShortOption(v)
  }

  implicit object SqlForTypeString extends SqlForType[String] {
    override def next(r: PositionedResult): String = r.nextString()
    override def nextO(r: PositionedResult): Option[String] = r.nextStringOption()
    override def set(p: PositionedParameters, v: String): Unit = p.setString(v)
    override def setO(p: PositionedParameters, v: Option[String]): Unit = p.setStringOption(v)
  }

  implicit class PositionedResultExt(val r: PositionedResult) extends AnyVal {
    def nextTagged[T <: TaggedType](implicit S: SqlForType[T#U], TC: TaggedTypeCtor[T]): T =
      TC(S.next(r))

    def nextTaggedO[T <: TaggedType](implicit S: SqlForType[T#U], TC: TaggedTypeCtor[T]): Option[T] =
      S.nextO(r).map(TC)
  }

  def GR_Tagged[T <: TaggedType](implicit S: SqlForType[T#U], TC: TaggedTypeCtor[T]): GetResult[T] =
    GetResult(_.nextTagged[T])

  def GR_TaggedO[T <: TaggedType](implicit S: SqlForType[T#U], TC: TaggedTypeCtor[T]): GetResult[Option[T]] =
    GetResult(_.nextTaggedO[T])

  def SP_Tagged[T <: TaggedType](implicit S: SqlForType[T#U]): SetParameter[T] = new SetParameter[T] {
    def apply(v: T, pp: PositionedParameters): Unit = S.set(pp, v.value)
  }

  def SP_TaggedO[T <: TaggedType](implicit S: SqlForType[T#U]): SetParameter[Option[T]] = new SetParameter[Option[T]] {
    def apply(v: Option[T], pp: PositionedParameters): Unit = S.setO(pp, v.map(_.value))
  }

  def sqlAccessors[T <: TaggedType](implicit S: SqlForType[T#U], TC: TaggedTypeCtor[T]) =
    (GR_Tagged[T], GR_TaggedO[T], SP_Tagged[T], SP_TaggedO[T])

//  def SP_TaggedLongL[T <: TaggedType]: SetParameter[List[T]] = new SetParameter[List[T]] {
//    def apply(v: List[T], pp: PositionedParameters): Unit = {
//      val sb = new StringBuilder
//      sb append '{'
//      if (v.nonEmpty) {
//        sb append v.head.longValue
//        v.tail.foreach(t => {
//          sb append ','
//          sb append t.longValue
//        })
//      }
//      sb append '}'
//
//      val o = pgObject("_int8", sb.toString)
//      pp.setObject(o, java.sql.Types.OTHER)
//    }
//  }

  // JsonStr is a special case because it is generic
  def GR_Json[T]: GetResult[JsonStr[T]] = implicitly[GetResult[String]].andThen(JsonStr[T])
  def GR_JsonO[T]: GetResult[Option[JsonStr[T]]] = implicitly[GetResult[Option[String]]].andThen(_ map JsonStr[T])
  def SP_Json[T]: SetParameter[JsonStr[T]] = new SetParameter[JsonStr[T]] {
    def apply(v: JsonStr[T], pp: PositionedParameters): Unit = {
      val jo = pgObject("json", v.value)
      pp.setObject(jo, java.sql.Types.OTHER)
    }
  }
  def SP_JsonO[T]: SetParameter[Option[JsonStr[T]]] = new SetParameter[Option[JsonStr[T]]] {
    def apply(o: Option[JsonStr[T]], pp: PositionedParameters): Unit = {
      val obj = o.map(v => pgObject("json", v.value))
      pp.setObjectOption(obj, java.sql.Types.OTHER)
    }
  }
  def sqlAccessorsJson[T] = (GR_Json[T], GR_JsonO[T], SP_Json[T], SP_JsonO[T])

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