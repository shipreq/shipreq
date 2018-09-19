package shipreq.webapp.base.util

import japgolly.microlibs.macro_utils._
import shipreq.webapp.base.protocol.MPickleMacroUtils
import boopickle._
import upickle._

object GenericDataMacros {
  def gdAllValues(d: GenericData, ctx: String): d.NonEmptyValues = macro GenericDataMacroImpls.quietAllValues
  def _gdAllValues(d: GenericData, ctx: String): d.NonEmptyValues = macro GenericDataMacroImpls.debugAllValues

  def gdUnequalValues(d: GenericData, ref: Any, ctx: String): d.Values = macro GenericDataMacroImpls.quietUnequalValues
  def _gdUnequalValues(d: GenericData, ref: Any, ctx: String): d.Values = macro GenericDataMacroImpls.debugUnequalValues

  def gdMPickler (d: GenericData, failUnrecognisedKeys: Boolean)(keys: d.Attr => String): d.ValueTypeClasses[ReadWriter] = macro GenericDataMacroImpls.quietMPickler
  def _gdMPickler(d: GenericData, failUnrecognisedKeys: Boolean)(keys: d.Attr => String): d.ValueTypeClasses[ReadWriter] = macro GenericDataMacroImpls.debugMPickler

  def binpickler (d: GenericData): d.ValueTypeClasses[Pickler] = macro GenericDataMacroImpls.quietBinPickler
  def _binpickler(d: GenericData): d.ValueTypeClasses[Pickler] = macro GenericDataMacroImpls.debugBinPickler
}

class GenericDataMacroImpls(val c: scala.reflect.macros.blackbox.Context) extends MacroUtils with MPickleMacroUtils {
  import c.universe._

  def resolveAttrsAndValues(debug: Boolean)(D: SingleType): Vector[(ModuleSymbol, ClassSymbol)] = {
    val attrTrait  = D.member(TypeName("Attr")).asType.toType
    val valueTrait = D.member(TypeName("Value")).asType.toType

    var attrs  = Vector.empty[ModuleSymbol]
    var values = Vector.empty[ClassSymbol]
    for (m <- D.members) {
      if (m.isModule) {
        val mm = m.asModule
        if (mm.moduleClass.asType.toType <:< attrTrait)
          attrs :+= mm
      } else if (m.isClass && !m.isAbstract) {
        val mc = m.asClass
        if (mc.asType.toType <:< valueTrait)
          values :+= mc
      }
    }
    attrs = attrs.sortBy(_.name.toString)
    values = values.sortBy(_.name.toString)

    if (debug) {
      println(s"Attrs: $attrs")
      println(s"Values: $values")
    }
    if (attrs.length != values.length)
      fail(s"attrs.length != values.length\n$attrs\n$values")

    attrs zip values
  }

  def attrDataType(attr: ModuleSymbol): Type =
    attr.moduleClass.asType.toType.decl(TypeName("Data")).asType.toType.dealias

  // ===================================================================================================================

  private def localNameToExpr(ctx: c.Expr[String]): String => RefTree =
    Some(readMacroArg_string(ctx)).filter(_.nonEmpty) match {
      case None      => n => Ident(TermName(n))
      case Some(pre) => n => Select(Ident(TermName(pre)), TermName(n))
    }

  def debugAllValues(d: c.Expr[GenericData], ctx: c.Expr[String]) = implAllValues(true )(d, ctx)
  def quietAllValues(d: c.Expr[GenericData], ctx: c.Expr[String]) = implAllValues(false)(d, ctx)
  def implAllValues(debug: Boolean)(d: c.Expr[GenericData], ctx: c.Expr[String]): c.Expr[d.value.NonEmptyValues] = {
    val D = d.actualType.asInstanceOf[SingleType]
    val attrsAndValues = resolveAttrsAndValues(debug)(D)
    val nameToExpr = localNameToExpr(ctx)

    val parts = for ((a,v) <- attrsAndValues) yield {
      val n = lowerCaseHead(a.name.toString)
      val g = nameToExpr(n)
      q"$a($g)"
    }

    val values = parts.foldLeft(q"$d.emptyValues": Tree)((a,b) => q"$a + $b")

    val impl = q"japgolly.microlibs.nonempty.NonEmpty.force($values)"

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[d.value.NonEmptyValues](impl)
  }

  def debugUnequalValues(d: c.Expr[GenericData], ref: c.Expr[Any], ctx: c.Expr[String]) = implUnequalValues(true )(d, ref, ctx)
  def quietUnequalValues(d: c.Expr[GenericData], ref: c.Expr[Any], ctx: c.Expr[String]) = implUnequalValues(false)(d, ref, ctx)
  def implUnequalValues(debug: Boolean)(d: c.Expr[GenericData], ref: c.Expr[Any], ctx: c.Expr[String]): c.Expr[d.value.Values] = {
    val D = d.actualType.asInstanceOf[SingleType]
    val attrsAndValues = resolveAttrsAndValues(debug)(D)
    val nameToExpr = localNameToExpr(ctx)

    // val equal = c.typeOf[Equal[_]]
    val stmts = for ((a,v) <- attrsAndValues) yield {
      val n      = lowerCaseHead(a.name.toString)
      val local  = nameToExpr(n)
      val refVal = q"$ref.${TermName(n)}"
      // We don't want to avoid creating new Equal instances on each invocation as this macro is used as a function.
      // If not, we could use Init().
      // val dt     = attrDataType(a)
      // val e      = needInferImplicit(appliedType(equal, dt))
      val e = q"$a.dataEquality"
      q"if (!$e.equal($refVal, $local)) us += $a($local)"
    }

    val impl = q""" {
        var us = $d.emptyValues
        ..${flattenBlocks(stmts.toList)}
        us
      } """

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[d.value.Values](impl)
  }
  // ===================================================================================================================

  def debugMPickler(d: c.Expr[GenericData], failUnrecognisedKeys: c.Expr[Boolean])(keys: c.Expr[d.value.Attr => String]) = implMPickler(true )(d, failUnrecognisedKeys)(keys)
  def quietMPickler(d: c.Expr[GenericData], failUnrecognisedKeys: c.Expr[Boolean])(keys: c.Expr[d.value.Attr => String]) = implMPickler(false)(d, failUnrecognisedKeys)(keys)
  def implMPickler(debug: Boolean)(d: c.Expr[GenericData], failUnrecognisedKeys: c.Expr[Boolean])(keys: c.Expr[d.value.Attr => String]): c.Expr[d.value.ValueTypeClasses[ReadWriter]] = {
    if (debug) println(sep)

    val D = d.actualType.asInstanceOf[SingleType]
    val DFQN = toSelectFQN(D.typeSymbol.asType)

    val keyLookup: List[(String, Literal)] =
      keys match {
        case Expr(Function(_, Match(_, caseDefs))) =>
          caseDefs map {
            case CaseDef(Select(_, name), _, key@ Literal(Constant(_: String))) => (name.toString, key)
            case x => fail(s"Expecting a case like: {case Attr => Literal}\nGot: ${showRaw(x)}")
          }
        case _ => fail(s"Expecting a function like: {case Attr => Literal}\nGot: ${showRaw(keys)}")
      }

    if (debug) println(s"Keys: $keyLookup")

    val attrsAndValues = resolveAttrsAndValues(debug)(D)

    var wCases   = List.empty[CaseDef]
    var rCases   = List.empty[CaseDef]
    var keysUsed = Set.empty[String]
    val init     = new Init("i$" + _)
    init += importMPickle

    for ((attr, value) <- attrsAndValues) {
      val name  = attr.name.toString
      val key   = keyLookup.find(_._1 == name).map(_._2) getOrElse fail(s"Key not found for $name.\nKeys = $keyLookup")
      val rw    = summonReadWriter(init, attrDataType(attr))
      wCases  ::= cq"v: $value => kvs :+= (($key, $rw write v.value))"
      rCases  ::= cq"$key => kvs += $attr apply $rw.read(kv._2)"
      keysUsed += key.toString()
    }

    if (keysUsed.size != attrsAndValues.size)
      fail(s"Keys must be unique: $keyLookup")

    val onUnrecognisedKey =
      if (readMacroArg_boolean(failUnrecognisedKeys))
        q"""sys.error("Unknown key '"+what+"' in "+o)"""
      else
        Literal(Constant(()))

    val impl = q""" {
      ..$init
      val empty = $DFQN.emptyValues
      val rwValues =
        ReadWriter[$DFQN.Values](
          vs => {
            var kvs = Vector.empty[(String, Js.Value)]
            vs.values foreach { case ..$wCases }
            Js.Obj(kvs: _*)
          },
          { case o: Js.Obj =>
            var kvs = empty
            o.value.foreach(kv =>
              kv._1 match {
                case ..$rCases
                case what => $onUnrecognisedKey
              }
            )
            kvs
          })
      val rwValue = rwValues.xmap(_.values.head)(empty + _)
      val rwNev   = rwValues.xmap[$DFQN.NonEmptyValues](japgolly.microlibs.nonempty.NonEmpty require_! _)(_.value)
      $DFQN.ValueTypeClasses[ReadWriter](rwValue, rwValues, rwNev)
    } """

    // ↑ rwValue isn't used so I don't care right now

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[d.value.ValueTypeClasses[ReadWriter]](impl)
  }

  // ===================================================================================================================

  def debugBinPickler(d: c.Expr[GenericData]) = implBinPickler(true )(d)
  def quietBinPickler(d: c.Expr[GenericData]) = implBinPickler(false)(d)
  def implBinPickler(debug: Boolean)(d: c.Expr[GenericData]): c.Expr[d.value.ValueTypeClasses[Pickler]] = {
    if (debug) println(sep)

    val D = d.actualType.asInstanceOf[SingleType]

    val attrsAndValues = resolveAttrsAndValues(debug)(D)

    val pickleCaseClass = Ident(TermName((if (debug) "_" else "") + "pickleCaseClass"))
    val init =
      for ((attr, value) <- attrsAndValues) yield {
        val p = TermName(c.freshName("p"))
        q"implicit val $p: Pickler[$value] = $pickleCaseClass": ValDef
      }

    val impl = q""" {
      import _root_.shipreq.webapp.base.protocol.BoopickleMacros._
      import $d._
      ..${flattenBlocks(init.toList)}
      implicit val value : Pickler[Value]          = pickleADT
      implicit val values: Pickler[Values]         = pickleIMap(emptyValues)
      implicit val nev   : Pickler[NonEmptyValues] = pickleNonEmptyMono[Values](values, implicitly)
      ValueTypeClasses(value, values, nev)
    } """

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[d.value.ValueTypeClasses[Pickler]](impl)
  }
}
