package shipreq.webapp.base.text

import japgolly.microlibs.macro_utils.WhiteboxMacroUtils
import scala.reflect.macros.whitebox.Context

object TextMacros {

  /** @param arg _ <: Text.Generic */
  def generateTypeclasses(arg: Any): Any = macro TextMacroImpls.typeclassImpl

  /** @param arg _ <: Text.Generic */
  def generateTypeclassesDebug(arg: Any): Any = macro TextMacroImpls.typeclassDebugImpl
}

class TextMacroImpls(val c: Context) extends WhiteboxMacroUtils {
  import c.universe._

  val atomFQN            = "shipreq.webapp.base.text.Atom.Base.Atom"
  val headingsFQN        = "shipreq.webapp.base.text.Atom.Headings"
  val plainTextMarkupFQN = "shipreq.webapp.base.text.Atom.PlainTextMarkup"

  val headingNo = "^heading(\\d)$".r

  /** Using an AtomTC, generate TC[Atom], TC[OptionalText], TC[NonEmptyText] for some text type T. */
  def typeclassImpl(arg: c.Expr[Any]) =
    _typeclassImpl(arg, false)

  /** Using an AtomTC, generate TC[Atom], TC[OptionalText], TC[NonEmptyText] for some text type T. */
  def typeclassDebugImpl(arg: c.Expr[Any]) =
    _typeclassImpl(arg, true)

  /** Using an AtomTC, generate TC[Atom], TC[OptionalText], TC[NonEmptyText] for some text type T. */
  private def _typeclassImpl(arg: c.Expr[Any], _debug: Boolean) = {

    def debug(msg: => Any): Unit =
      if (_debug)
        println(msg.toString)

    val tType = arg.actualType
    val tTerm = tType.termSymbol

    debug(sep)
    debug(s"tType = $tType")

    val atomTypes = tType.members.filter(t => t.isClass && t.isPublic && !t.isAbstract && t.asClass.baseClasses.exists(_.fullName == atomFQN)).toList
    val atomTypeNames = atomTypes.map(_.name)
    debug(s"atomTypes = $atomTypeNames")

    def getInnerType(parentFQN: String, typeMember: String, prefix: String): Option[String] = {
      val isSubType = tType.baseClasses.exists(_.fullName == parentFQN)
      Option.when(isSubType)(
        tType.member(TermName(typeMember))
          .typeSignature
          .finalResultType
          .toString
          .stripSuffix(".type")
          .replaceFirst("^.+\\.", "")
          .stripPrefix(prefix)
      )
    }

    val headingTitleType = getInnerType(headingsFQN, "headerTitle", "HeadingTitle")
    val styledInnerType  = getInnerType(plainTextMarkupFQN, "styled", "StyledInner")
    val isStyledInner    = tType.toString.contains("Text.StyledInner")
    debug(s"headingTitleType = $headingTitleType, styledInnerType = $styledInnerType, isStyledInner = $isStyledInner")

    var allVals     = Vector.empty[TermName]
    var valDefs     = Vector.empty[ValDef]
    var lazyValDefs = Vector.empty[ValDef]
    var cases       = Vector.empty[CaseDef]
    var needLI      = false
    val lazyNEA     = isStyledInner

    def mkLazy(v: ValDef): ValDef = {
      val ValDef(m, a, b, c) = v
      val m2 = Modifiers(Flag.LAZY `|` m.flags, m.privateWithin, m.annotations)
      ValDef(m2, a, b, c)
    }

    for (t <- atomTypeNames) {
      val nStr = lowerCaseHead(t.toString)
      val nTerm = TermName(nStr)
      var valLazy = false
      val atomTypeName = t.toTermName
      val body = nStr match {
        case "issue" =>
          valLazy = true
          q"a.$nTerm(t)(issue3._2)"
        case "unorderedList" =>
          valLazy = true
          needLI = true
          q"a.$nTerm(t)(li)"
        case headingNo(h) =>
          val ht = q"${TermName(s"headingTitle${headingTitleType.get}3")}._3"
          val fn = "heading" + h
          val term = TermName(fn)
          q"""this.$term(t)($ht)"""
        case "bold" | "italic" | "underline" | "strikethrough" =>
          val si =
            if (isStyledInner)
              q"${TermName("lazyNEA")}"
            else
              q"${TermName(s"styledInner${styledInnerType.get}3")}._3"
          q"""this.$nTerm(t)($si)"""
        case "emailAddress" | "literal" | "monospace" | "teX" | "webAddress" =>
          q"this.$nTerm(t)"
        case _ =>
          q"a.$nTerm(t)"
      }
      allVals :+= nTerm
      val valDef = q"val $nTerm: TC[t.Atom] = $body.asInstanceOf[TC[t.Atom]]": ValDef
      if (valLazy)
        lazyValDefs :+= mkLazy(valDef)
      else
        valDefs :+= valDef

      cases :+= cq"Atom.Type.$atomTypeName => $nTerm"
    }

    cases :+= {
      val errMsg = Literal(Constant(s"Text.${tType.typeSymbol.name} doesn't support atom type: "))
      cq"""t => throw new java.lang.IllegalArgumentException($errMsg + t)"""
    }

    val valMods = if (lazyValDefs.isEmpty) Modifiers() else Modifiers(Flag.LAZY)

    val li = if (!needLI) EmptyTree else
      q"lazy val li: TC[shipreq.base.util.NonEmptyArraySeq[t.ListItem]] = a.lazily(a.nea(a.arr(arr, implicitly[ClassTag[t.ListItem]]))(arr))"

    val declLazyNEA = if (lazyNEA) q"lazy val lazyNEA = a.lazily(nea)" else EmptyTree

    val impl = q""" {
      var atom: TC[$tTerm.Atom]         = null.asInstanceOf[TC[$tTerm.Atom]        ]
      var arr : TC[$tTerm.OptionalText] = null.asInstanceOf[TC[$tTerm.OptionalText]]
      var nea : TC[$tTerm.NonEmptyText] = null.asInstanceOf[TC[$tTerm.NonEmptyText]]
      $declLazyNEA
      val t = $tTerm
      val ct = implicitly[ClassTag[t.Atom]]
      ..$valDefs
      ..$lazyValDefs
      $valMods val get: Atom.Type => TC[t.Atom] = {case ..$cases}
      $valMods val all: List[TC[t.Atom]] = ${allVals.foldRight(q"Nil": Tree)((v, l) => q"$v :: $l")}
      atom = a.sum(t)(get, all)
      arr  = a.arr(atom, ct)
      $li
      nea = a.nea(arr)(atom)
      (atom, arr, nea)
    } """

    debug("\n" + showCode(impl) + "\n")

    c.Expr[Any](impl)
  }
}
