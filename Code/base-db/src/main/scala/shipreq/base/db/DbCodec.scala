package shipreq.base.db

import scala.reflect.macros.blackbox.Context
import slick.jdbc.{GetResult, PositionedParameters, SetParameter}
import shipreq.base.macros.MacroUtils
import shipreq.base.util.TaggedTypes.JsonStr
import SqlHelpers._
import DbCodecT.Removed

/**
  * Means of reading and writing a value to the database.
  *
  * Two type params are required so that G can be covariant so that the implicit conversions to GetResult work.
  * If G is invariant, Scala's stupid implicit search fails.
  */
class DbCodecT[+G, -S](val get: GetResult[G], val set: SetParameter[S]) {
  def xmap[B](f: G => B)(g: B => S): DbCodec[B] =
    DbCodec(get andThen f, set contramap g)

  def withOption[GG >: G, SS <: S](o: DbCodecT[Option[GG], Option[SS]]): DbCodec.WithOptionT[GG, SS] =
    new DbCodec.WithOptionT(this, o)

  def readOnly: DbCodecT[G, Removed] =
    new DbCodecT(get, null)

  def writeOnly: DbCodecT[Removed, S] =
    new DbCodecT(null, set)
}

object DbCodecT {
  sealed trait Removed

  def apply[A](get: GetResult[A], set: SetParameter[A]): DbCodec[A] =
    new DbCodec(get, set)

  def summon[A](implicit g: GetResult[A], s: SetParameter[A]): DbCodec[A] =
    DbCodec(g, s)

  def const[A](a: A): DbCodec[A] =
    DbCodec(GetResult const a, SetNothing)

  def json[A]: DbCodec[JsonStr[A]] = {
    def GR_Json[T]: GetResult[JsonStr[T]] = implicitly[GetResult[String]].andThen(JsonStr[T])
    def SP_Json[T]: SetParameter[JsonStr[T]] = new SetParameter[JsonStr[T]] {
      def apply(v: JsonStr[T], pp: PositionedParameters): Unit = {
        val jo = pgObject("json", v.value)
        pp.setObject(jo, java.sql.Types.OTHER)
      }
    }
    DbCodec(GR_Json[A], SP_Json[A])
  }

  def caseClass[A]: DbCodec[A] = macro DbCodecMacros.quietCaseClass[A]
  def _caseClass[A]: DbCodec[A] = macro DbCodecMacros.debugCaseClass[A]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type WithOption[A] = WithOptionT[A, A]

  final class WithOptionT[+G, -S](sole: DbCodecT[G, S], val option: DbCodecT[Option[G], Option[S]])
      extends DbCodecT[G, S](sole.get, sole.set) {

    override def xmap[B](f: G => B)(g: B => S): WithOption[B] =
      super.xmap(f)(g) withOption option.xmap(_ map f)(_ map g)

    override def readOnly: WithOptionT[G, Removed] =
      new WithOptionT(sole.readOnly, new DbCodecT(option.get, null))

    override def writeOnly: WithOptionT[Removed, S] =
      new WithOptionT(sole.writeOnly, new DbCodecT(null, option.set))
  }

  object WithOption {
    def summon[A](implicit g: GetResult[A], s: SetParameter[A], go: GetResult[Option[A]], so: SetParameter[Option[A]]): DbCodec.WithOption[A] =
      DbCodecT(g, s) withOption DbCodecT(go, so)

    def const[A](a: A): WithOption[A] =
      DbCodecT.const(a) withOption DbCodecT.const(Option(a))

    def json[A]: WithOption[JsonStr[A]] = {
      def GR_JsonO[T]: GetResult[Option[JsonStr[T]]] = implicitly[GetResult[Option[String]]].andThen(_ map JsonStr[T])
      def SP_JsonO[T]: SetParameter[Option[JsonStr[T]]] = new SetParameter[Option[JsonStr[T]]] {
        def apply(o: Option[JsonStr[T]], pp: PositionedParameters): Unit = {
          val obj = o.map(v => pgObject("json", v.value))
          pp.setObjectOption(obj, java.sql.Types.OTHER)
        }
      }
      DbCodecT.json[A] withOption DbCodecT(GR_JsonO[A], SP_JsonO[A])
    }

    def caseClass[A]: DbCodec.WithOption[A] = macro DbCodecMacros.quietCaseClassO[A]
    def _caseClass[A]: DbCodec.WithOption[A] = macro DbCodecMacros.debugCaseClassO[A]
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

class DbCodecMacros(val c: Context) extends MacroUtils {
  import c.universe._

  def quietCaseClass[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implCaseClass[T](false)
  def debugCaseClass[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implCaseClass[T](true)
  def implCaseClass[T: c.WeakTypeTag](debug: Boolean): c.Expr[DbCodec[T]] = {
    val T      = concreteWeakTypeOf[T]
    val apply  = tcApplyFn(T)
    val params = primaryConstructorParams(T)

    val implInner =
      params match {
        case Nil =>
          q"DbCodecT.const[$T]($apply())"

//        case param :: Nil =>
//          val (n, t) = nameAndType(T, param)
//          q"DbCodec.summon[$t].xmap[$T]($apply)(_.$n)"

        case _ =>
          var codecs    = Vector.empty[ValDef]
          var getFields = Vector.empty[Tree]
          var setFields = Vector.empty[Tree]

          for (p <- params) {
            val (n, t) = nameAndType(T, p)
            val cg = TermName(c.freshName())
            val cs = TermName(c.freshName())
            codecs    :+= q"val $cg = implicitly[GetResult[$t]]"
            codecs    :+= q"val $cs = implicitly[SetParameter[$t]]"
            getFields :+= q"$cg(rs)"
            setFields :+= q"$cs(value.$n, pp)"
          }

          q"""
            import _root_.scala.slick.jdbc.{GetResult, SetParameter}
            ..$codecs
            DbCodec[$T](
              GetResult[$T](rs => $apply(..$getFields)),
              SetParameter[$T]((value,pp) => {..$setFields}))
          """
      }

    val impl = q"""
        import _root_.shipreq.base.db.SqlHelpers._
        $implInner
      """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec[T]](impl)
  }


  def quietCaseClassO[T: c.WeakTypeTag]: c.Expr[DbCodec.WithOption[T]] = implCaseClassO[T](false)
  def debugCaseClassO[T: c.WeakTypeTag]: c.Expr[DbCodec.WithOption[T]] = implCaseClassO[T](true)
  def implCaseClassO[T: c.WeakTypeTag](debug: Boolean): c.Expr[DbCodec.WithOption[T]] = {
    val T      = concreteWeakTypeOf[T]
    val apply  = tcApplyFn(T)
    val params = primaryConstructorParams(T)

    val implInner =
      params match {
        case Nil =>
          q"DbCodec.WithOption.const[$T]($apply())"

        case param :: Nil =>
          val (n, t) = nameAndType(T, param)
          q"DbCodec.WithOption.summon[$t].xmap[$T]($apply)(_.$n)"

        case _ =>
          fail("Unsupported.")
      }

    val impl = q"""
        import _root_.shipreq.base.db.SqlHelpers._
        $implInner
      """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec.WithOption[T]](impl)
  }

}
