package shipreq.webapp.base.protocol

import japgolly.microlibs.macro_utils.MacroUtils
import japgolly.univeq.UnivEq
import scala.reflect.macros.blackbox.Context
import boopickle._

object BoopickleMacros {

  def xmap[A, B](ba: B => A)(ab: A => B)(implicit p: Pickler[B]): Pickler[A] =
    p.xmap(ba)(ab)

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

  final def  pickleObject[T]: Pickler[T] = macro BoopickleMacroImpls.quietObject[T]
  final def _pickleObject[T]: Pickler[T] = macro BoopickleMacroImpls.debugObject[T]

  final def  pickleCaseClass[T]: Pickler[T] = macro BoopickleMacroImpls.quietCaseClass[T]
  final def _pickleCaseClass[T]: Pickler[T] = macro BoopickleMacroImpls.debugCaseClass[T]

  final def  pickleADT[T]: Pickler[T] = macro BoopickleMacroImpls.quietADT[T]
  final def _pickleADT[T]: Pickler[T] = macro BoopickleMacroImpls.debugADT[T]

  final def  derivePickler[T]: Pickler[T] = macro BoopickleMacroImpls.quietDerive[T]
  final def _derivePickler[T]: Pickler[T] = macro BoopickleMacroImpls.debugDerive[T]
}

// =====================================================================================================================
class BoopickleMacroImpls(val c: Context) extends MacroUtils {
  import BoopickleMacros._
  import c.universe._

  def PicklerType(t: Type): Type =
    appliedType(c.typeOf[Pickler[_]], t)

  def quietObject[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implObject[T](false)
  def debugObject[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implObject[T](true)
  def implObject[T: c.WeakTypeTag](debug: Boolean): c.Expr[Pickler[T]] = {
    val T = concreteWeakTypeOf[T]
    val t = T.termSymbol

    if (!t.isModule)
      fail(s"$t is not an object.")

    val impl = q"_root_.boopickle.ConstPickler($t)"

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Pickler[T]](impl)
  }

  private def newPickler(T: Type, pickleImpl: Tree, unpickleImpl: Tree): Tree =
    q"""
      new Pickler[$T] {
        override def pickle(value: $T)(implicit state: PickleState): Unit = {$pickleImpl}
        override def unpickle(implicit state: UnpickleState): $T = {$unpickleImpl}
      }
    """

  def quietCaseClass[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implCaseClass[T](false)
  def debugCaseClass[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implCaseClass[T](true)
  def implCaseClass[T: c.WeakTypeTag](debug: Boolean): c.Expr[Pickler[T]] = {
    val T          = concreteWeakTypeOf[T].dealias
    val params     = primaryConstructorParams(T)
    lazy val apply = tcApplyFn(T)

    val impl =
      params match {
        case Nil =>
          val t = T.typeSymbol
          if (t.isModuleClass)
            q"_root_.boopickle.ConstPickler[$T](${toSelectFQN(t.asType)})"
          else
            q"_root_.boopickle.ConstPickler[$T]($apply())"

        case param :: Nil =>
          val (n, t) = nameAndType(T, param)
          q"_root_.shipreq.webapp.base.protocol.BoopickleMacros.xmap[$T,$t]($apply)(_.$n)"

        case _ =>
          val init = Init()
          var pickleFields   = Vector.empty[Tree]
          var unpickleFields = Vector.empty[Tree]

          for (p <- params) {
            val (n, t) = nameAndType(T, p)
            val fp = init.valImp(PicklerType(t))
            pickleFields   :+= q"state.pickle(value.$n)($fp)"
            // pickleFields   :+= q"""{println("PICKLING: " + value.$n); state.pickle(value.$n)($fp)}"""
            unpickleFields :+= q"state.unpickle($fp)"
          }

          def pickleImpl = q"..$pickleFields"

          def unpickleImpl = {
             q"$apply(..$unpickleFields)"
//            val i2 = Init()
//            val terms = unpickleFields.map(t => q"""{val x = $t; println("UNPICKLED: " + x); x}""").map(i2.valDef)
//            q"{..$i2; $apply(..$terms)}"
          }

          q""" {
            import _root_.boopickle.{Pickler, PickleState, UnpickleState}
            ..$init
            ${newPickler(T, pickleImpl, unpickleImpl)}
          } """
      }

    if (debug) println("\n" + showCode(impl) + "\n")
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
//      fail(s"Failed to resolve the following: $ignored")


  def quietADT[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implADT[T](false, false)
  def debugADT[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implADT[T](true, false)
  def implADT[T: c.WeakTypeTag](debug: Boolean, derive: Boolean): c.Expr[Pickler[T]] = {
    val T     = weakTypeOf[T]
    val types = findConcreteTypesNE(T, LeavesOnly)
                  .toList.sortBy(_.fullName)
                  .map(t => determineAdtType(T, t))

    var picklerNames = Vector.empty[TermName]
    var picklers     = Vector.empty[ValDef]
    var cases        = Vector.empty[CaseDef]

    var index = 0
    val picklerType =
    for (t <- types) {
      val t2 = fixAdtTypeForCaseDef(t)
      val fp = TermName(c.freshName())
      val picklerImpl: Tree =
        if (derive)
          tryInferImplicit(PicklerType(t)) getOrElse implDerive(false)(c.WeakTypeTag(t)).tree
        else
          needInferImplicit(PicklerType(t))

      picklerNames :+= fp
      picklers :+= q"val $fp = _root_.shipreq.webapp.base.protocol.BinCodecGeneric.PicklerExt($picklerImpl).unsafeWiden[$T]"
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
          import _root_.boopickle.Pickler
          ..$picklers
          val all = Array[Pickler[$T]](..$picklerNames)
          def index(t: $T): Int = t match {case ..$cases}
          new _root_.shipreq.webapp.base.protocol.BoopickleMacros.Selector[$T](all, index)
        """
      }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Pickler[T]](impl)
  }

  def quietDerive[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implDerive[T](false)
  def debugDerive[T: c.WeakTypeTag]: c.Expr[Pickler[T]] = implDerive[T](true)
  def implDerive[T: c.WeakTypeTag](debug: Boolean): c.Expr[Pickler[T]] = {
    val T = weakTypeOf[T]
    if (T.dealias.typeSymbol.isAbstract)
      implADT(debug, true)
    else
      implCaseClass(debug)
  }
}
