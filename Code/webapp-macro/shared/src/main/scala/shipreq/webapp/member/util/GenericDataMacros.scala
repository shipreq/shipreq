package shipreq.webapp.member.project.util

import japgolly.microlibs.compiletime._

object GenericDataMacros {
  def gdAllValues(d: GenericData, ctx: String): d.NonEmptyValues = macro GenericDataMacroImpls.quietAllValues
  def _gdAllValues(d: GenericData, ctx: String): d.NonEmptyValues = macro GenericDataMacroImpls.debugAllValues

  def gdUnequalValues(d: GenericData, ref: Any, ctx: String): d.Values = macro GenericDataMacroImpls.quietUnequalValues
  def _gdUnequalValues(d: GenericData, ref: Any, ctx: String): d.Values = macro GenericDataMacroImpls.debugUnequalValues

  def gdUnequalValues2(d: GenericData, ref: Any, vs: Any): d.Values = macro GenericDataMacroImpls.quietUnequalValues2
  def _gdUnequalValues2(d: GenericData, ref: Any, vs: Any): d.Values = macro GenericDataMacroImpls.debugUnequalValues2
}

class GenericDataMacroImpls(val c: scala.reflect.macros.blackbox.Context) extends MacroUtils {
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

    // val equal = c.typeOf[Eq[_]]
    val stmts = for ((a,v) <- attrsAndValues) yield {
      val n      = lowerCaseHead(a.name.toString)
      val local  = nameToExpr(n)
      val refVal = q"$ref.${TermName(n)}"
      // We don't want to avoid creating new Equal instances on each invocation as this macro is used as a function.
      // If not, we could use Init().
      // val dt     = attrDataType(a)
      // val e      = needInferImplicit(appliedType(equal, dt))
      val e = q"$a.dataEquality"
      q"if (!$e.eqv($refVal, $local)) us += $a($local)"
    }

    val impl = q""" {
        var us = $d.emptyValues
        ..${flattenBlocks(stmts.toList)}
        us
      } """

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[d.value.Values](impl)
  }

  def debugUnequalValues2(d: c.Expr[GenericData], ref: c.Expr[Any], vs: c.Expr[Any]) = implUnequalValues2(true )(d, ref, vs)
  def quietUnequalValues2(d: c.Expr[GenericData], ref: c.Expr[Any], vs: c.Expr[Any]) = implUnequalValues2(false)(d, ref, vs)
  def implUnequalValues2(debug: Boolean)(d: c.Expr[GenericData], ref: c.Expr[Any], vs: c.Expr[Any]): c.Expr[d.value.Values] = {
    val D = d.actualType.asInstanceOf[SingleType]
    val attrsAndValues = resolveAttrsAndValues(debug)(D)

    val stmts = for ((a,v) <- attrsAndValues) yield {
      val n      = lowerCaseHead(a.name.toString)
      val refVal = q"$ref.${TermName(n)}"
      val e      = q"$a.dataEquality"
      q"$a.get($vs).foreach(v => if ($e.eqv($refVal, v.value)) vs2 -= $a)"
    }

    val impl = q""" {
        var vs2: $d.Values = $vs
        ..${flattenBlocks(stmts.toList)}
        vs2
      } """

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[d.value.Values](impl)
  }
}
