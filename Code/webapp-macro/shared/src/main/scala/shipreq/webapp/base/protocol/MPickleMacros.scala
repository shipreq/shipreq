package shipreq.webapp.base.protocol

import japgolly.microlibs.macro_utils.MacroUtils
import scala.reflect.macros.blackbox.Context
import upickle._

object MPickleMacros {

  def  caseClass1[T]: ReadWriter[T] = macro MPickleMacroImpls.quietCaseClass1[T]
  def _caseClass1[T]: ReadWriter[T] = macro MPickleMacroImpls.debugCaseClass1[T]

  def  caseClassAsArray[T](arrayOrder: Symbol*): ReadWriter[T] = macro MPickleMacroImpls.quietCaseClassAsArray[T]
  def _caseClassAsArray[T](arrayOrder: Symbol*): ReadWriter[T] = macro MPickleMacroImpls.debugCaseClassAsArray[T]

  /**
   * ADT to a String and/or Object.
   */
  def  pickleAdtOS[T](keys: T => String): ReadWriter[T] = macro MPickleMacroImpls.quietAdtOS[T]
  def _pickleAdtOS[T](keys: T => String): ReadWriter[T] = macro MPickleMacroImpls.debugAdtOS[T]

  /**
   * ADT to a Number.
   */
  def  pickleAdtN[T](keys: T => Int): ReadWriter[T] = macro MPickleMacroImpls.quietAdtN[T]
  def _pickleAdtN[T](keys: T => Int): ReadWriter[T] = macro MPickleMacroImpls.debugAdtN[T]
}

// =====================================================================================================================

trait MPickleMacroUtils { self: MacroUtils =>
  import c.universe._

  def importMPickle =
    q"import _root_.upickle._"

  def summonR(i: Init, tot: TypeOrTree) = tot match {
    case GotType(t) => i valImp appliedType(typeOf[Reader[_]], t)
    case GotTree(t) => i valImp tq"_root_.upickle.Reader[$t]"
  }

  def summonW(i: Init, tot: TypeOrTree) = tot match {
    case GotType(t) => i valImp appliedType(typeOf[Writer[_]], t)
    case GotTree(t) => i valImp tq"_root_.upickle.Writer[$t]"
  }

  def summonReadWriter(i: Init, tot: TypeOrTree) = tot match {
    case GotType(t) => i valImp appliedType(typeOf[ReadWriter[_]], t)
    case GotTree(t) => i valImp tq"_root_.upickle.ReadWriter[$t]"
  }

  def summonRW(i: Init, t: TypeOrTree) =
    (summonR(i, t), summonW(i, t))

  def newReadWriter(i: Init, t: Type)(write: Tree, read: Tree): Tree =
    i.wrap(q"ReadWriter[$t]($write, $read)")

  def writeToJsonStr(writer: TermName, value: Tree): Tree =
    q"_root_.upickle.json.write($writer.write($value))"

  def readFromJsonStr(reader: TermName, value: Tree): Tree =
    q"$reader.read(_root_.upickle.json read $value)"

  final type SymStrPair = (scala.Symbol, String)
}

class MPickleMacroImpls(val c: Context) extends MacroUtils with MPickleMacroUtils {
  import c.universe._

  def quietCaseClass1[T: c.WeakTypeTag]: c.Expr[ReadWriter[T]] = implCaseClass1[T](false)
  def debugCaseClass1[T: c.WeakTypeTag]: c.Expr[ReadWriter[T]] = implCaseClass1[T](true)
  def implCaseClass1[T: c.WeakTypeTag](debug: Boolean): c.Expr[ReadWriter[T]] = {
    val T      = concreteWeakTypeOf[T]
    val TC     = T.typeSymbol.companion
    val param  = primaryConstructorParams_require1(T)
    val init   = new Init("i$" + _); init += importMPickle
    val (n, t) = nameAndType(T, param)
    val vr     = summonR(init, t)
    val vw     = summonW(init, t)

    val impl: Tree =
      // invokeReadJs will break PF composition because lhs will always match
      init.wrap(q"ReadWriter[$T](j => $vw.write(j.$n), $vr.read.andThen($TC.apply))")

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[ReadWriter[T]](impl)
  }

  def quietCaseClassAsArray[T: c.WeakTypeTag](arrayOrder: c.Expr[scala.Symbol]*): c.Expr[ReadWriter[T]] = implCaseClassAsArray[T](arrayOrder, false)
  def debugCaseClassAsArray[T: c.WeakTypeTag](arrayOrder: c.Expr[scala.Symbol]*): c.Expr[ReadWriter[T]] = implCaseClassAsArray[T](arrayOrder, true)
  def implCaseClassAsArray[T: c.WeakTypeTag](arrayOrderArgs: Seq[c.Expr[scala.Symbol]], debug: Boolean): c.Expr[ReadWriter[T]] = {
    val T      = concreteWeakTypeOf[T]
    val TC     = T.typeSymbol.companion
    val params = primaryConstructorParams(T)
    val init   = new Init("i$" + _)
    init += importMPickle

    def invokeWriteJs(subj: TermName, param: Symbol) = {
      val (n, t) = nameAndType(T, param)
      val w = summonW(init, t)
      q"$w.write($subj.$n)"
    }
    def invokeReadJs(subj: TermName, param: Symbol) = {
      val t = nameAndType(T, param)._2
      val r = summonR(init, t)
      q"$r.read($subj)"
    }

    val impl: Tree =
      params match {
        case Nil =>
          fail("Class constructor has no parameters, codec not required.")

        case _ :: Nil =>
          fail("Use caseClass1.")

        case _ =>
          val arrayOrder: Vector[String] = arrayOrderArgs.map(readMacroArg_symbol).toVector
          if (arrayOrder.toSet.size != arrayOrderArgs.size)
            fail(s"Duplicate field name in: $arrayOrder")
          if (arrayOrder.size != params.size)
            fail(s"Expected ${params.size} keys, got ${arrayOrder.size}.\n  Fields: $params\n  Keys: $arrayOrder")
          val i = TermName("i")
          var writes = Vector.empty[Tree]
          var reads = Vector.fill(arrayOrder.length)(null: Tree)
          var nextChar = 'a'.toInt
          val paramsAndIdxByName = params.zipWithIndex.map(p => p._1.asTerm.name.toString -> p).toMap
          val vals = arrayOrder map { fieldName =>
            val (p, pi) = paramsAndIdxByName.getOrElse(fieldName, sys error s"Field not found: $fieldName")
            val v = TermName(nextChar.toChar.toString)
            nextChar += 1
            writes :+= invokeWriteJs(i, p)
            reads = reads.updated(pi, invokeReadJs(v, p))
            pq"$v"
          }
          val rCases = cq"Js.Arr(..$vals) => $TC(..$reads)"
          init.wrap(q"ReadWriter[$T](i => Js.Arr(..$writes), {case $rCases} )")
      }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[ReadWriter[T]](impl)
  }

  def debugAdtOS[T: c.WeakTypeTag](keys: c.Expr[T => String]) = implAdtOS(true )(keys)
  def quietAdtOS[T: c.WeakTypeTag](keys: c.Expr[T => String]) = implAdtOS(false)(keys)
  def implAdtOS[T: c.WeakTypeTag](debug: Boolean)(keys: c.Expr[T => String]): c.Expr[ReadWriter[T]] = {
    if (debug) println(sep)

    val T     = weakTypeOf[T]
    val types = findConcreteAdtTypesNE(T, LeavesOnly).toVector
    if (debug) println("TYPES: " + types)

    val keyCases = readMacroArg_tToLitFn(keys)
    if (debug) println(s"Keys: $keyCases")

    var defaultCase = Option.empty[(Tree, CaseDef)]
    var wCases      = Vector.empty[CaseDef]
    var roCases     = Vector.empty[CaseDef]
    var rsCases     = Vector.empty[CaseDef]
    var unseenTypes = types
    val init        = new Init("i$" + _)
    init += importMPickle

    def defaultNotAllowedHere() = fail("Only a class can have an empty key (i.e. be a default).")

    for ((te, key) <- keyCases) {
      val keyIsBlank = key match {case Literal(Constant(s: String)) => s.isEmpty}
      te match {

        // case Obj => key
        case Left(s) =>
          if (keyIsBlank) defaultNotAllowedHere()
          val t = s.tpe
          val (matchedTypes, remaining) = unseenTypes.partition(_ <:< t)
          unseenTypes = remaining
          if (matchedTypes.isEmpty)
            fail(s"A type you specified doesn't match any cases: $t")

          wCases  :+= cq"$s => Js.Str($key)"
          rsCases :+= cq"$key => $s"

        // case _: Type => key
        case Right(t) =>
          val (matchedTypes, remaining) = unseenTypes.partition(_ <:< t)
          unseenTypes = remaining
          if (matchedTypes.isEmpty)
            fail(s"A type you specified doesn't match any cases: $t")

          if (matchedTypes.length == 1 && primaryConstructorParams(t).isEmpty) {
            // Zero-arg case class
            if (keyIsBlank) defaultNotAllowedHere()
            val apply = tcApplyFn(t)
            val v     = init.valDef(q"$apply(): $t")
            wCases  :+= cq"_: $t => Js.Str($key)"
            rsCases :+= cq"$key  => $v"

          } else {
            // Pickle class
            val (vr, vw) = summonRW(init, t)
            if (keyIsBlank) {
              if (defaultCase.isDefined)
                fail("Cannot have more than one default.")
              if (debug)
                println(s"Default case: $t")
              val wc: CaseDef = cq"v: $t => $vw write v"
              val rc          = q"$vr.read"
              defaultCase = Some((rc, wc))
            } else {
              wCases  :+= cq"v: $t => Js.Obj(($key, $vw write v))"
              roCases :+= cq"$key  => $vr read v"
            }
          }
      }
    }

    if (unseenTypes.nonEmpty)
      fail(s"The following types do not have keys: $unseenTypes")

    // Merge read cases
    var rCases = Vector.empty[CaseDef]
    if (rsCases.nonEmpty)
      rCases :+= cq"Js.Str(x) => x match {case ..$rsCases}"
    if (roCases.nonEmpty)
      rCases :+= cq"Js.Obj(x) => val v = x._2; x._1 match {case ..$roCases}"

    // Install default case
    var rFn = q"{ case ..$rCases }: PartialFunction[Js.Value, $T]"
    for ((r, w) <- defaultCase) {
      wCases = w +: wCases
      rFn = if (rCases.isEmpty)
          q"$r: PartialFunction[Js.Value, $T]"
        else {
          // PFComposition benchmarks suggest this (tmpvar) is faster. Probably bullshit but doesn't hurt.
          val next = init.valDef(rFn)
          q"$r.orElse($next): PartialFunction[Js.Value, $T]"
        }
    }
    val wFn = q"{ case ..$wCases }"

    val impl = newReadWriter(init, T)(wFn, rFn)

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[ReadWriter[T]](impl)
  }

  def debugAdtN[T: c.WeakTypeTag](keys: c.Expr[T => Int]) = implAdtN(true )(keys)
  def quietAdtN[T: c.WeakTypeTag](keys: c.Expr[T => Int]) = implAdtN(false)(keys)
  def implAdtN[T: c.WeakTypeTag](debug: Boolean)(keys: c.Expr[T => Int]): c.Expr[ReadWriter[T]] = {
    if (debug) println(sep)

    val T     = weakTypeOf[T]
    val types = findConcreteAdtTypesNE(T, LeavesOnly).toVector
    if (debug) println("TYPES: " + types)

    val keyCases = readMacroArg_tToLitFn(keys)
    if (debug) println(s"Keys: $keyCases")

    var wCases      = Vector.empty[CaseDef]
    var rCases      = Vector.empty[CaseDef]
    var unseenTypes = types
    val init        = new Init("i$" + _)
    init += importMPickle

    for ((te, key) <- keyCases)
      te match {

        // case Obj => key
        case Left(s) =>
          val t = s.tpe
          val (matchedTypes, remaining) = unseenTypes.partition(_ <:< t)
          unseenTypes = remaining
          if (matchedTypes.isEmpty)
            fail(s"A type you specified doesn't match any cases: $t")

          wCases :+= cq"$s => Js.Num($key)"
          rCases :+= cq"$key => $s"

        // case _: Type => key
        case Right(t) =>
          val (matchedTypes, remaining) = unseenTypes.partition(_ <:< t)
          unseenTypes = remaining
          if (matchedTypes.isEmpty)
            fail(s"A type you specified doesn't match any cases: $t")

          if (matchedTypes.length == 1 && primaryConstructorParams(t).isEmpty) {
            // Zero-arg case class
            val apply = tcApplyFn(t)
            val v     = init.valDef(q"$apply(): $t")
            wCases  :+= cq"_: $t => Js.Num($key)"
            rCases  :+= cq"$key  => $v"

          } else
            fail(s"Invalid type: $t")
      }

    if (unseenTypes.nonEmpty)
      fail(s"The following types do not have keys: $unseenTypes")

    val impl = newReadWriter(init, T)(q"{ case ..$wCases }", q"{ case Js.Num(n) => n.toInt match {case ..$rCases}}")

    if (debug) println("\n" + showCode(impl) + "\n" + sep)

    c.Expr[ReadWriter[T]](impl)
  }
}
