package com.beardedlogic.shipreq.db

import org.postgresql.util.PGobject
import scala.slick.jdbc.{SetParameter, GetResult}
import scala.slick.session.{PositionedResult, PositionedParameters}
import com.beardedlogic.shipreq.util.TypeTags._

object SqlHelpers {

  @inline def pgObject(typ: String, value: String): PGobject = {
    val o = new PGobject()
    o.setType(typ)
    o.setValue(value)
    o
  }

  implicit class PositionedResultExt(val r: PositionedResult) extends AnyVal {
    def nextId[T <: JLong @@ TypeTag[JLong]](): T = r.nextObject.asInstanceOf[T]
    def nextId_?[T <: JLong @@ TypeTag[JLong]](): Option[T] = r.nextObjectOption.asInstanceOf[Option[T]]
    def nextTShort[Tag <: TypeTag[JShort]](): JShort @@ Tag = r.nextShort.tag[Tag]
    def nextTShort_?[Tag <: TypeTag[JShort]](): Option[JShort @@ Tag] = r.nextShortOption.map(_.tag[Tag])
  }

  def GR_TaggedString[T <: TypeTag[String]]: GetResult[String @@ T] = GetResult(_.nextString.tag[T])
  def GR_TaggedStringOpt[T <: TypeTag[String]]: GetResult[Option[String @@ T]] = GetResult(_.nextStringOption.tagInner[T])
  def SP_TaggedString[Tag <: TypeTag[String]]: SetParameter[String @@ Tag] = new SetParameter[String @@ Tag] {
    def apply(v: String @@ Tag, pp: PositionedParameters): Unit = pp.setString(v)
  }

  def GR_TaggedLong[T <: JLong @@ TypeTag[JLong]]: GetResult[T] = GetResult(_.nextId[T])
  def SP_TaggedLong[T <: JLong @@ TypeTag[JLong]]: SetParameter[T] = new SetParameter[T] {
    def apply(v: T, pp: PositionedParameters): Unit = pp.setLong(v)
  }
  def GR_TaggedLongOpt[T <: JLong @@ TypeTag[JLong]]: GetResult[Option[T]] = GetResult(_.nextId_?[T])
  def SP_TaggedLongOpt[T <: JLong @@ TypeTag[JLong]] = new SetParameter[Option[T]] {
    def apply(v: Option[T], pp: PositionedParameters): Unit = pp.setObjectOption(v, java.sql.Types.BIGINT)
  }
  def SP_TaggedLongArray[T <: JLong @@ TypeTag[JLong]]: SetParameter[List[T]] = new SetParameter[List[T]] {
    def apply(v: List[T], pp: PositionedParameters): Unit = {
      val sb = new StringBuilder
      sb append '{'
      if (v.nonEmpty) {
        sb append v.head.longValue
        v.tail.foreach(t => {
          sb append ','
          sb append t.longValue
        })
      }
      sb append '}'

      val o = pgObject("_int8", sb.toString)
      pp.setObject(o, java.sql.Types.OTHER)
    }
  }


  def GR_Json[T]: GetResult[Json[T]] = GetResult(_.nextString.tag[IsJsonFor[T]])
  def SP_Json[T]: SetParameter[Json[T]] = new SetParameter[Json[T]] {
    def apply(v: Json[T], pp: PositionedParameters): Unit = {
      val jo = pgObject("json", v)
      pp.setObject(jo, java.sql.Types.OTHER)
    }
  }

  def GR_TaggedShort[Tag <: TypeTag[JShort]]: GetResult[JShort @@ Tag] = GetResult(_.nextTShort[Tag])
  def SP_TaggedShort[Tag <: TypeTag[JShort]]: SetParameter[JShort @@ Tag] = new SetParameter[JShort @@ Tag] {
    def apply(v: JShort @@ Tag, pp: PositionedParameters): Unit = pp.setShort(v)
  }

  private[this] val LeadingWhitespace = """[\r\n]+\s*""".r

  implicit class SqlStringExt(val s: String) extends AnyVal {
    def sql = LeadingWhitespace.replaceAllIn(s, " ").trim
    def inTable(table: String) = {
      val p = table + "."
      """(^|,)\s*""".r.replaceAllIn(s, _.group(0)+p)
    }
  }
}