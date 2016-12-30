package shipreq.base.db

import doobie.imports._
import doobie.enum.{jdbctype => JT}
import doobie.free.{preparedstatement => PS, resultset => RS}
import java.time.Duration
import org.postgresql.util.{PGInterval, PGobject}
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe.TypeTag
import scalaz.NonEmptyList
import shipreq.base.macros.MacroUtils
import shipreq.base.util.TaggedTypes.JsonStr
import shipreq.base.util.ScalaExt._

object SqlHelpers {

  private[this] val SqlComments = """\s+--[^\r\n]*""".r
  private[this] val LeadingWhitespace = """[\r\n]+\s*""".r

  implicit class SqlStringExt(private val s: String) extends AnyVal {
    def unNull(default: String): String =
      if (s eq null) default else s

    def sql: String = {
      var t = s
      t = SqlComments.replaceAllIn(t, "")
      t = LeadingWhitespace.replaceAllIn(t, " ")
      t.trim
    }
  }

  implicit class DoobieMetaExt[A](private val self: Meta[A]) extends AnyVal {
    def readOnlyAnyVal[B](f: A => B)(implicit tt: TypeTag[B]): Meta[B] =
      self.xmap[B](f, _ => sys error s"Writing $tt not supported.")

    def readOnly[B >: Null](f: A => B)(implicit tt: TypeTag[B], ev: Null <:< A): Meta[B] =
      self.nxmap[B](f, _ => sys error s"Writing $tt not supported.")

    def writeOnlyAnyVal[B](f: B => A)(implicit tt: TypeTag[B]): Meta[B] =
      self.xmap[B](_ => sys error s"Reading $tt not supported.", f)

    def writeOnly[B >: Null](f: B => A)(implicit tt: TypeTag[B], ev: Null <:< A): Meta[B] =
      self.nxmap[B](_ => sys error s"Reading $tt not supported.", f)
  }

  implicit class DoobieCompositeExt[A](private val self: Composite[A]) extends AnyVal {
    def readOnly[B](f: A => B)(implicit tt: TypeTag[B]): Composite[B] =
      self.xmap(f, _ => sys error s"Writing $tt not supported.")

    def writeOnly[B](f: B => A)(implicit tt: TypeTag[B]): Composite[B] =
      self.xmap(_ => sys error s"Reading $tt not supported.", f)
  }


  def doobieMetaCaseClass[A]: Meta[A] = macro SqlMacros.quietCaseClass1[A]
  def _doobieMetaCaseClass[A]: Meta[A] = macro SqlMacros.debugCaseClass1[A]

  def pgObject(typ: String, value: String): PGobject = {
    val o = new PGobject()
    o.setType(typ)
    o.setValue(value)
    o
  }

// Doobie really really really doesn't allow NULLs.
//  implicit val doobieMetaInteger: Meta[java.lang.Integer] =
//    Meta.advanced[java.lang.Integer](
//      NonEmptyList(JT.Integer, JT.TinyInt, JT.SmallInt, JT.BigInt),
//      NonEmptyList("int1", "int2", "int4"),
//      _.getObject(_).asInstanceOf[Integer],
//      PS.setObject(_, _, java.sql.Types.INTEGER),
//      RS.updateObject(_, _, java.sql.Types.INTEGER))

  /** @param value [0..255] */
  final case class PGChar(value: Char) extends AnyVal {
    def toByte: Byte =
      value.toByte
    def toPGobject: PGobject =
      pgObject("char", value.toString)
  }
  object PGChar {
    implicit val metaPGChar: Meta[PGChar] =
      Meta.advanced[PGChar](
        NonEmptyList(JT.Char),
        NonEmptyList("char"),
        (rs, i) => PGChar(rs.getString(i).head),
        (i, a) => PS.setObject(i, a.toPGobject, java.sql.Types.OTHER),
        (i, a) => RS.updateObject(i, a.toPGobject, java.sql.Types.OTHER)
      )
  }

  val doobieMetaChar: Meta[Char] =
    Meta[PGChar].xmap[Char](_.value, PGChar.apply)

  val doobieMetaJson = Meta.other[PGobject]("json")

  def jsonStr[A: TypeTag]: Meta[JsonStr[A]] =
    doobieMetaJson.xmap[JsonStr[A]](
      o => JsonStr(if (o == null) null else o.getValue),
      j => pgObject("json", j.value))

  implicit val doobieMetaDuration: Meta[Duration] =
    Meta.other[PGInterval]("interval").nxmap(
      i => sys.error("Reading a PGInterval into a Duration is not yet supported."),
      d => new PGInterval(
        0, 0, 0, 0, 0, // years, months, days, hours, minutes
        d.getSeconds.toDouble + d.getNano/1000000000.0
      )
    )

}

// =====================================================================================================================
final class SqlMacros(val c: Context) extends MacroUtils {
  import c.universe._

  def quietCaseClass1[T: c.WeakTypeTag]: c.Expr[Meta[T]] = implCaseClass1[T](false)
  def debugCaseClass1[T: c.WeakTypeTag]: c.Expr[Meta[T]] = implCaseClass1[T](true)
  def implCaseClass1[T: c.WeakTypeTag](debug: Boolean): c.Expr[Meta[T]] = {
    val T      = concreteWeakTypeOf[T]
    val apply  = tcApplyFn(T)
    val params = primaryConstructorParams(T)

    val impl = params match {
      case param :: Nil =>
        val (n, t) = nameAndType(T, param)
        q"Meta[$t].xmap[$T]($apply, _.$n)"

      case _ =>
        fail(s"Can't create a Meta[$T]. Expected exactly 1 field on, found ${params.size}.")
    }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Meta[T]](impl)
  }
}