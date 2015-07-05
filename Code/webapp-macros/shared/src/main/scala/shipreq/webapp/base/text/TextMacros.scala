package shipreq.webapp.base.text

object TextMacros {
  import shipreq.webapp.macros.MacroUtils._
  import scala.reflect.macros.whitebox.Context

  val atomFQN = "shipreq.webapp.base.text.Atom.Base.Atom"

  /**
   * Using an AtomTC, generate TC[Atom], TC[OptionalText], TC[NonEmptyText] for some text type T.
   */
  def typeclassImpl(c: Context)(arg: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    def debug(msg: => Any): Unit =
      () //println(msg.toString)

    val tType = arg.actualType
    val tTerm = tType.termSymbol

    debug("_" * 120)
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

      val caseDef = cq"_: t.${t.toTypeName} => $nTerm"
      cases :+= caseDef
    }

    val valMods = if (lazyValDefs.isEmpty) Modifiers() else Modifiers(Flag.LAZY)

    val li = if (!needLI) EmptyTree else
      q"lazy val li: TC[NonEmptyVector[t.ListItem]] = a.lazily(a.nev(a vec vec)(vec))"

    val impl = q""" {
      val t = $tTerm
      ..$valDefs
      ..$lazyValDefs
      $valMods val all = Vector[TC[t.Atom]](..$allVals)
      $valMods val choose: t.Atom => TC[t.Atom] = {case ..$cases}
      $valMods val atom = a.sum(t)(choose, all)
      $valMods val vec  = a.vec(atom)
      $li
      val nev = a.nev(vec)(atom)
      (atom, vec, nev): (TC[$tTerm.Atom], TC[$tTerm.OptionalText], TC[$tTerm.NonEmptyText])
    } """

    debug("\n" + impl + "\n")

    c.Expr[Any](impl)
  }

  /**
   * @param arg T <: Text.Generic
   */
  def generateTypeclasses(arg: Any): Any = macro typeclassImpl
}
