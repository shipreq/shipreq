package shipreq.webapp.base.protocol

import scala.reflect.macros.blackbox.Context
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.macros.MacroUtils._
import boopickle._

object BoopickleMacros {

  def xmap[A, B](ba: B => A)(ab: A => B)(implicit p: Pickler[B]): Pickler[A] =
    p.xmap(ba)(ab)

  def lazily[A](f: => Pickler[A]): Pickler[A] = {
    lazy val p = f
    new Pickler[A] {
      override def pickle(a: A)(implicit state: PickleState): Unit = p.pickle(a)
      override def unpickle(implicit state: UnpickleState): A = p.unpickle
    }
  }

  def enum[V: UnivEq](nev: NonEmptyVector[V]): Pickler[V] =
    new Pickler[V] {
      val vs = nev.whole
      val vtoi = vs.zipWithIndex.toMap
      assert(vtoi.size == nev.length, s"Duplicates found in $nev")
      override def pickle(v: V)(implicit state: PickleState): Unit = {
        val i = vtoi(v)
        state.enc.writeInt(i)
      }
      override def unpickle(implicit state: UnpickleState): V =
        state.dec.readIntCode match {
          case Right(i) => vs(i)
          case Left(_)  => throw new IllegalArgumentException("Unknown coding")
      }
    }

  /**
   * MAKE SURE index REFLECTS ORDER OF picklers!
   */
  def unsafeSelector[T](picklers: Pickler[_ <: T]*)(index: T => Int): Pickler[T] =
    new Selector[T](picklers.map(_.asInstanceOf[Pickler[T]]).toArray, index)

  class Selector[T](all: Array[Pickler[T]], index: T => Int) extends Pickler[T] {
    override def pickle(value: T)(implicit state: PickleState): Unit = {
      val i = index(value)
      state.enc.writeInt(i)
      all(i).pickle(value)
    }
    override def unpickle(implicit state: UnpickleState): T = {
      val i = state.dec.readInt
      all(i).unpickle
    }
  }

  implicit class AnyRefPicklerExt[A <: AnyRef](private val p: Pickler[A]) extends AnyVal {
    def reuseByUnivEq(implicit ev: UnivEq[A]) = new PickleWithReuse[A](p, true)
    def reuseByRef = new PickleWithReuse[A](p, false)
  }

  final class PickleWithReuse[A <: AnyRef](p: Pickler[A], byUnivEq: Boolean) extends Pickler[A] {
    private[this] val getP: (PickleState, A) => Option[Int] = if (byUnivEq) _ immutableRefFor _  else _ identityRefFor _
    private[this] val getU: (UnpickleState, Int) => A       = if (byUnivEq) _.immutableFor[A](_) else _.identityFor[A](_)
    private[this] val setP: (PickleState  , A) => Unit      = if (byUnivEq) _ addImmutableRef _  else _ addIdentityRef _
    private[this] val setU: (UnpickleState, A) => Unit      = if (byUnivEq) _ addImmutableRef _  else _ addIdentityRef _

    override def pickle(value: A)(implicit state: PickleState): Unit = {
      val ref = getP(state, value)
      if (ref.isDefined)
        state.enc.writeInt(-ref.get)
      else {
        state.enc.writeInt(0)
        p.pickle(value)
        setP(state, value)
      }
    }
    override def unpickle(implicit state: UnpickleState): A =
      state.dec.readIntCode match {
        case Right(i) =>
          if (i == 0) {
            val value = p.unpickle
            setU(state, value)
            value
          } else
            getU(state, -i)
        case Left(_) =>
          throw new IllegalArgumentException("Unknown coding")
      }
  }

  import BoopickleMacroImpls._

  final def pickleObject [T]: Pickler[T] = macro quietObject[T]
  final def _pickleObject[T]: Pickler[T] = macro debugObject[T]

  final def pickleCaseClass [T]: Pickler[T] = macro quietCaseClass[T]
  final def _pickleCaseClass[T]: Pickler[T] = macro debugCaseClass[T]

  final def pickleADT [T]: Pickler[T] = macro quietADT[T]
  final def _pickleADT[T]: Pickler[T] = macro debugADT[T]
}

// =====================================================================================================================

object BoopickleMacroImpls {
  import BoopickleMacros._

  def quietObject[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = implObject[T](c, false)
  def debugObject[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = implObject[T](c, true)

  def quietCaseClass[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = implCaseClass[T](c, false)
  def debugCaseClass[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = implCaseClass[T](c, true)

  def quietADT[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = implADT[T](c, false)
  def debugADT[T: c.WeakTypeTag](c: Context): c.Expr[Pickler[T]] = implADT[T](c, true)

  def implObject[T: c.WeakTypeTag](c: Context, debug: Boolean): c.Expr[Pickler[T]] = {
    import c.universe._

    val T = concreteWeakTypeOf[T](c)
    val t = T.termSymbol

    if (!t.isModule)
      fail(c, s"$t is not an object.")

    val impl = q"ConstPickler($t)"

    if (debug) println("\n" + impl + "\n")
    c.Expr[Pickler[T]](impl)
  }

  private def newPickler(c: Context)(T: c.universe.Type, pickleImpl: c.universe.Tree, unpickleImpl: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""
      new Pickler[$T] {
        override def pickle(value: $T)(implicit state: PickleState): Unit = {$pickleImpl}
        override def unpickle(implicit state: UnpickleState): $T = {$unpickleImpl}
      }
    """
  }

  def implCaseClass[T: c.WeakTypeTag](c: Context, debug: Boolean): c.Expr[Pickler[T]] = {
    import c.universe._

    val T      = concreteWeakTypeOf[T](c)
    val apply  = tcApplyFn(c)(T)
    val params = primaryConstructorParams(c)

    val impl =
      params match {
        case Nil =>
          q"ConstPickler[$T]($apply())"

        case param :: Nil =>
          val (n, t) = nameAndType(c)(param)
          q"xmap[$T,$t]($apply)(_.$n)"

        case _ =>
          var fieldPicklers  = Vector.empty[ValDef]
          var pickleFields   = Vector.empty[Tree]
          var unpickleFields = Vector.empty[Tree]

          for (p <- params) {
            val (n, t) = nameAndType[T](c)(p)
            val fp = TermName(c.freshName())
            fieldPicklers  :+= q"val $fp = implicitly[Pickler[$t]]"
            pickleFields   :+= q"state.pickle(value.$n)($fp)"
            unpickleFields :+= q"state.unpickle($fp)"
            // pickleFields   :+= q"state.pickle[$t](value.$n)"
            // unpickleFields :+= q"state.unpickle[$t]"
          }

          def pickleImpl = q"..$pickleFields"
          def unpickleImpl = q"$apply(..$unpickleFields)"

          q""" {
            ..$fieldPicklers
            ${newPickler(c)(T, pickleImpl, unpickleImpl)}
          } """
      }

    if (debug) println("\n" + impl + "\n")
    c.Expr[Pickler[T]](impl)
  }

//    val ctx = T match {
//      case ExistentialType(_, TypeRef(pre, _, _)) => pre
//      case TypeRef(pre, _, _) => pre
//    }
//
//    var ignored: List[ClassSymbol] = Nil
//    var pathDependent = false
//
//      if (t2 <:< T)
//        q"$q.addConcreteType[$t2]"
//      else {
//        // Check if type is a direct path-dependent type
//        val tname = t.toType.typeSymbol.name.toTypeName
//        val child = ctx.member(tname)
//        if (child != NoSymbol) {
//          pathDependent = true
//          val full = Select(Ident(ctx.termSymbol), tname)
//          q"$q.addConcreteType[$full]"
//        } else {
//          // Assume ok for now
//          ignored ::= t
//          q
//        }
//      }
//    }
//
//    if (ignored.nonEmpty && !pathDependent)
//      fail(c, s"Failed to resolve the following: $ignored")


  def implADT[T: c.WeakTypeTag](c: Context, debug: Boolean): c.Expr[Pickler[T]] = {
    import c.universe._

    val T     = weakTypeOf[T]
    val types = findConcreteTypesNE(c)(T, LeavesOnly)
                  .toList.sortBy(_.fullName)
                  .map(t => determineAdtType(c)(T, t))

    var picklerNames = Vector.empty[TermName]
    var picklers     = Vector.empty[ValDef]
    var cases        = Vector.empty[CaseDef]

    var index = 0
    for (t <- types) {

      val t2: Tree =
        if (t.typeSymbol.isModuleClass)
          TypeTree(t)
        else {
          // Take the FQN and re-evaluate. Why? I don't know.
          // But without this there'll be spurious exhaustiveness warnings
          val fqn = t.typeSymbol.asClass.fullName
          val tmp = selectFQN(c)(fqn)
          if (t.typeArgs.isEmpty)
            tmp
          else
            AppliedTypeTree(tmp, t.typeArgs.map(TypeTree(_)))
        }

      val fp = TermName(c.freshName())
      picklerNames :+= fp
      picklers :+= q"val $fp = implicitly[Pickler[$t]].asInstanceOf[Pickler[$T]]"
      val ci = cq"_: $t2 => $index"
      cases :+= ci
      index += 1
    }

    val impl =
      if (types.lengthCompare(1) == 0) {
        val p = picklerNames.head
        q"${picklers.head}; $p"
      } else {
        q"""
          ..$picklers
          val all = Array[Pickler[$T]](..$picklerNames)
          def index(t: $T): Int = t match {case ..$cases}
          new Selector[$T](all, index)
        """
      }

    if (debug) println("\n" + impl + "\n")
    c.Expr[Pickler[T]](impl)
  }
}
