package shipreq.webapp.base.text

import japgolly.microlibs.macro_utils.WhiteboxMacroUtils
import scala.reflect.macros.whitebox.Context

object TextMacros {

  /**
   * @param arg T <: Text.Generic
   */
  def generateTypeclasses(arg: Any): Any = macro TextMacroImpls.typeclassImpl
}

class TextMacroImpls(val c: Context) extends WhiteboxMacroUtils {
  import c.universe._

  val atomFQN = "shipreq.webapp.base.text.Atom.Base.Atom"

  /**
   * Using an AtomTC, generate TC[Atom], TC[OptionalText], TC[NonEmptyText] for some text type T.
   */
  def typeclassImpl(arg: c.Expr[Any]) = {

    def debug(msg: => Any): Unit =
      () //println(msg.toString)

    val tType = arg.actualType
    val tTerm = tType.termSymbol

    debug(sep)
    debug(s"tType = $tType")

    val atomTypes = tType.members.filter(t => t.isClass && t.isPublic && !t.isAbstract && t.asClass.baseClasses.exists(_.fullName == atomFQN)).toList
    val atomTypeNames = atomTypes.map(_.name)
    debug(s"atomTypes = $atomTypeNames")

    var allVals     = Vector.empty[TermName]
    var valDefs     = Vector.empty[ValDef]
    var lazyValDefs = Vector.empty[ValDef]
    var cases       = Vector.empty[CaseDef]
    var needLI      = false

    def mkLazy(v: ValDef): ValDef = {
      val ValDef(m, a, b, c) = v
      val m2 = Modifiers(Flag.LAZY `|` m.flags, m.privateWithin, m.annotations)
      ValDef(m2, a, b, c)
    }

    for (t <- atomTypeNames) {
      val nStr = lowerCaseHead(t.toString)
      val nTerm = TermName(nStr)
      var valLazy = false
      val body = nStr match {
        case "issue" =>
          valLazy = true
          q"a.$nTerm(t)(issue3._2)"
        case "unorderedList" =>
          valLazy = true
          needLI = true
          q"a.$nTerm(t)(li)"
        case _ =>
          q"a.$nTerm(t)"
      }
      allVals :+= nTerm
      val valDef = q"val $nTerm: TC[t.Atom] = $body.asInstanceOf[TC[t.Atom]]": ValDef
      if (valLazy)
        lazyValDefs :+= mkLazy(valDef)
      else
        valDefs :+= valDef

      cases :+= cq"Atom.Type.${t.toTermName} => $nTerm"
    }

    cases :+= {
      val errMsg = Literal(Constant(s"Text.${tType.typeSymbol.name} doesn't support atom type: "))
      cq"""t => throw new java.lang.IllegalArgumentException($errMsg + t)"""
    }

    val valMods = if (lazyValDefs.isEmpty) Modifiers() else Modifiers(Flag.LAZY)

    val li = if (!needLI) EmptyTree else
      q"lazy val li: TC[shipreq.base.util.NonEmptyArraySeq[t.ListItem]] = a.lazily(a.nea(a.arr(arr, implicitly[ClassTag[t.ListItem]]))(arr))"

    val impl = q""" {
      val t = $tTerm
      val ct = implicitly[ClassTag[t.Atom]]
      ..$valDefs
      ..$lazyValDefs
      $valMods val get: Atom.Type => TC[t.Atom] = {case ..$cases}
      $valMods val all: List[TC[t.Atom]] = ${allVals.foldRight(q"Nil": Tree)((v, l) => q"$v :: $l")}
      $valMods val atom = a.sum(t)(get, all)
      $valMods val arr  = a.arr(atom, ct)
      $li
      val nea = a.nea(arr)(atom)
      (atom, arr, nea): (TC[$tTerm.Atom], TC[$tTerm.OptionalText], TC[$tTerm.NonEmptyText])
    } """

    debug("\n" + showCode(impl) + "\n")

    c.Expr[Any](impl)
  }
}
