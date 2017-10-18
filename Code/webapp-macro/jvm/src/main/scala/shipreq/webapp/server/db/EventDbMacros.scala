package shipreq.webapp.server.db

import japgolly.microlibs.macro_utils.MacroUtils
import scala.reflect.macros.blackbox.Context
import shipreq.base.db.SqlHelpers.PGChar
import shipreq.webapp.base.protocol.MPickleMacroUtils
import shipreq.base.util.TaggedTypes.TaggedInt

/**
 * Read/write an Event from/to the `event` table, namely the columns:
 * - `data_id_type`
 * - `data_id`
 * - `data`
 *
 * @tparam E An event.
 */
class DbCodec[E](val read : (Byte, Option[Int], String) => E,
                 val write: E => (PGChar, Option[Int], String))

object DbCodec {
  final class MonoId[A](val byte: Byte, val int: A => Int, val make: Int => A) {
    val bytePG = PGChar(byte.toChar)
  }
  def monoId[A <: TaggedInt](byte: Byte, make: Int => A): MonoId[A] =
    new MonoId(byte, _.value, make)

  final case class PolyId[A](bytePG: A => PGChar, int: A => Int, make: Byte => Int => A)
  def  polyId[A]: DbCodec.PolyId[A] = macro EventDbMacroImpls.quietPolyId[A]
  def _polyId[A]: DbCodec.PolyId[A] = macro EventDbMacroImpls.debugPolyId[A]

  class Registry[R, W](val reader: Short => DbCodec[R], val writer: W => (Short, DbCodec[W]))
  def  registry[R, W <: R](typeIds: R => Short): DbCodec.Registry[R, W] = macro EventDbMacroImpls.quietRegistry[R, W]
  def _registry[R, W <: R](typeIds: R => Short): DbCodec.Registry[R, W] = macro EventDbMacroImpls.debugRegistry[R, W]
}

object EventDbMacros {
  final val noDataIdType: Byte = ' '
  val noDataIdTypePG = PGChar(noDataIdType.toChar)

  /** Codec for event with 1 field, which will go in the `data_id` column. */
  def dbCodecIdOnly [E]: DbCodec[E] = macro EventDbMacroImpls.quietDbCodecIdOnly[E]
  def _dbCodecIdOnly[E]: DbCodec[E] = macro EventDbMacroImpls.debugDbCodecIdOnly[E]

  /** Codec for event with 1 field, which will go in the `data` column. */
  def dbCodecDataOnly [E]: DbCodec[E] = macro EventDbMacroImpls.quietDbCodecDataOnly[E]
  def _dbCodecDataOnly[E]: DbCodec[E] = macro EventDbMacroImpls.debugDbCodecDataOnly[E]

  /** Codec for event with 2 fields. */
  def dbCodec2 [E]: DbCodec[E] = macro EventDbMacroImpls.quietDbCodec2[E]
  def _dbCodec2[E]: DbCodec[E] = macro EventDbMacroImpls.debugDbCodec2[E]

  def dbCodecJust [E](keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.quietDbCodecJust[E]
  def _dbCodecJust[E](keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.debugDbCodecJust[E]

  def dbCodecIdAnd [E](keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.quietDbCodecIdAnd[E]
  def _dbCodecIdAnd[E](keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.debugDbCodecIdAnd[E]

  def dbCodecIdGdAnd [E](gd: Symbol, keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.quietDbCodecIdGdAnd[E]
  def _dbCodecIdGdAnd[E](gd: Symbol, keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.debugDbCodecIdGdAnd[E]
}

// =====================================================================================================================
object EventDbMacroImpls {
  import upickle._

  def readPickledObject(d: String): Js.Obj =
    json.read(d) match {
      case o: Js.Obj => o
      case x         => sys.error("Object required. Got: " + x)
    }
}

class EventDbMacroImpls(val c: Context) extends MacroUtils with MPickleMacroUtils {
  import c.universe._

  private def implicitMono(t: Type): Option[Tree] =
    tryInferImplicit(appliedType(c.typeOf[DbCodec.MonoId[_]], t))

  private def implicitPoly(t: Type): Option[Tree] =
    tryInferImplicit(appliedType(c.typeOf[DbCodec.PolyId[_]], t))

  type IdFns = (Tree, Tree, Tree)
  private def implicitIdFns(t: Type): IdFns = {
    def tryMono: Option[IdFns] =
      implicitMono(t).map(f => (q"$f.bytePG", q"$f int a", q"$f make i"))
    def tryPoly: Option[IdFns] =
      implicitPoly(t).map(f => (q"$f bytePG a", q"$f int a", q"$f.make(b)(i)"))
    tryMono orElse tryPoly getOrElse fail(s"No implicit MonoId or PolyId instance found for: $t")
  }

  def quietPolyId[T: c.WeakTypeTag]: c.Expr[DbCodec.PolyId[T]] = implPolyId[T](false)
  def debugPolyId[T: c.WeakTypeTag]: c.Expr[DbCodec.PolyId[T]] = implPolyId[T](true)
  def implPolyId[T: c.WeakTypeTag](debug: Boolean): c.Expr[DbCodec.PolyId[T]] = {
    val T     = weakTypeOf[T]
    val types = findConcreteTypesNE(T, LeavesOnly)

    val init      = Init()
    var byteCases = Vector.empty[CaseDef]
    var intCases  = Vector.empty[CaseDef]
    var makeCases = Vector.empty[CaseDef]

    init += q"var tmp = Set.empty[Byte]"

    for (tc <- types) {
      val t       = tc.asType.toType
      val t2      = fixAdtTypeForCaseDef(t)
      val mono    = implicitMono(t) getOrElse fail(s"MonoId not found for: $t")
      val b       = init.valDef(q"$mono.byte")
      init += q"tmp += $b"
      byteCases :+= cq"_: $t2 => $mono.bytePG"
      intCases  :+= cq"i: $t2 => $mono int i"
      makeCases :+= cq"`$b` => $mono.make"
    }

    val errPre = s"$T: "
    init += q"""def err(e: String) = sys.error($errPre + e + "\nValid data_id_types: "+tmp)"""
    init += q"""if (tmp.size != ${types.size}) sys.error($errPre + "Duplicate data_id_type found in: " + tmp)"""
    byteCases :+= cq"""x => err("Invalid data_id_type in byte(): " + x)"""
    makeCases :+= cq"""x => err("Invalid data_id_type in make(): " + x)"""
    intCases  :+= cq"""x => err("Invalid data_id: " + x)"""

    val impl = q"""
      ..$init
      new DbCodec.PolyId[$T]({case ..$byteCases}, {case ..$intCases}, {case ..$makeCases})
    """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec.PolyId[T]](impl)
  }

  private def writeDataOnly(T: Type)(data: Tree) =
    q""" (e: $T) => (noDataIdTypePG, None, $data) """

  private def writeIdAnd(T: Type)(idField: TermName, idByte: Tree, idInteger: Tree)(data: Tree) =
    q"""
      (e: $T) => {
        val a = e.$idField
        ($idByte, $idInteger, $data)
      }
    """

  def quietDbCodecIdOnly[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implDbCodecIdOnly[T](false)
  def debugDbCodecIdOnly[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implDbCodecIdOnly[T](true)
  def implDbCodecIdOnly[T: c.WeakTypeTag](debug: Boolean): c.Expr[DbCodec[T]] = {
    val T        = concreteWeakTypeOf[T]
    val apply    = tcApplyFn(T)
    val f        = primaryConstructorParams_require1(T)
    val (fn, ft) = nameAndType(T, f)
    val init     = Init()

    val (idByte, idInteger, idMake) = implicitIdFns(ft)
    val writeFn = writeIdAnd(T)(fn, idByte, q"Some($idInteger)")(Literal(Constant(null)))

    val impl = q"..$init; new DbCodec[$T]((b, ii, _) => {$getDataId; $apply($idMake)}, $writeFn)"

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec[T]](impl)
  }

  def quietDbCodecDataOnly[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implDbCodecDataOnly[T](false)
  def debugDbCodecDataOnly[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implDbCodecDataOnly[T](true)
  def implDbCodecDataOnly[T: c.WeakTypeTag](debug: Boolean): c.Expr[DbCodec[T]] = {
    val T         = concreteWeakTypeOf[T]
    val apply     = tcApplyFn(T)
    val f         = primaryConstructorParams_require1(T)
    val init      = Init()
    val (fn, ft)  = nameAndType(T, f)
    val (fr, fw)  = summonRW(init, ft)
    val readData  = readFromJsonStr(fr, q"d")
    val writeData = writeToJsonStr(fw, q"e.$fn")
    val writeFn   = writeDataOnly(T)(writeData)

    val impl = q"""
      ..$init
      new DbCodec[$T](
        (_, _, d) => $apply($readData),
        $writeFn)
    """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec[T]](impl)
  }

  def quietDbCodec2[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implDbCodec2[T](false)
  def debugDbCodec2[T: c.WeakTypeTag]: c.Expr[DbCodec[T]] = implDbCodec2[T](true)
  def implDbCodec2[T: c.WeakTypeTag](debug: Boolean): c.Expr[DbCodec[T]] = {
    val T          = concreteWeakTypeOf[T]
    val apply      = tcApplyFn(T)
    val (f1, f2)   = primaryConstructorParams_require2(T)
    val init       = Init()
    val (f1n, f1t) = nameAndType(T, f1)
    val (f2n, f2t) = nameAndType(T, f2)
    val (f2r, f2w) = summonRW(init, f2t)

    val (idByte, idInteger, idMake) = implicitIdFns(f1t)
    val writeFn = writeIdAnd(T)(f1n, idByte, q"Some($idInteger)")(writeToJsonStr(f2w, q"e.$f2n"))
    val readData = readFromJsonStr(f2r, q"d")

    val impl = q"""
      ..$init
      new DbCodec[$T]((b, ii, d) => {$getDataId; $apply($idMake, $readData)}, $writeFn)
    """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec[T]](impl)
  }

  def quietDbCodecJust   [T: c.WeakTypeTag]                          (keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecPlusKeys[T](false)(false, None,     keys)
  def debugDbCodecJust   [T: c.WeakTypeTag]                          (keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecPlusKeys[T](true )(false, None,     keys)
  def quietDbCodecIdAnd  [T: c.WeakTypeTag]                          (keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecPlusKeys[T](false)(true,  None,     keys)
  def debugDbCodecIdAnd  [T: c.WeakTypeTag]                          (keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecPlusKeys[T](true )(true,  None,     keys)
  def quietDbCodecIdGdAnd[T: c.WeakTypeTag](gd: c.Expr[scala.Symbol], keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecPlusKeys[T](false)(true,  Some(gd), keys)
  def debugDbCodecIdGdAnd[T: c.WeakTypeTag](gd: c.Expr[scala.Symbol], keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecPlusKeys[T](true )(true,  Some(gd), keys)

  def implDbCodecPlusKeys[T: c.WeakTypeTag](debug: Boolean)(hasId: Boolean, gdArg: Option[c.Expr[scala.Symbol]], keys: Seq[c.Expr[SymStrPair]]): c.Expr[DbCodec[T]] = {
    val T      = concreteWeakTypeOf[T]
    val apply  = tcApplyFn(T)
    val params = primaryConstructorParams(T).toVector

    val keyLookup: Map[String, Literal] =
      keys map readMacroArg_symbolString toMap

    if (debug) println(s"KeyLookup: $keyLookup")

    var ps = params map (nameAndType(T, _))
    if (hasId) // ID expected to be first
      ps = ps.tail

    val expKeySize = ps.length - gdArg.size
    if (keyLookup.size != expKeySize)
      fail(s"Expected $expKeySize keys, got ${keyLookup.size}.\n  Fields: $ps\n  Keys: $keyLookup")

    val gdNT = gdArg.map { arg =>
      val gd = readMacroArg_symbol(arg)
      ps.find(_._1.toString == gd)
        .getOrElse(fail(s"GenericData field $gd not found. Available: ${ps.map(_._1.toString)}"))
    }

    type TT = Tree => Tree
    val init  = Init(importMPickle)
    var dataW = Vector.empty[TT => Tree]
    var dataR = Vector.empty[Tree]

    for ((pn, pt) <- ps)
      gdNT match {
        case Some((gn, gt)) if gn eq pn =>
          val gr = summonR(init, gt)
          dataR :+= q"$gr read o"

        case _ =>
          val (vr, vw) = summonRW(init, pt)
          val kn = pn.decodedName.toString
          keyLookup.get(kn) match {

            // Mandatory field (merge fields)
            case Some(Literal(Constant(""))) =>
              dataW :+= ((add: TT) => q"""
                val o = $vw.write(e.$pn)
                assert(o.isInstanceOf[Js.Obj], "Js.Obj expected: " + o)
                if (o != null) o.asInstanceOf[Js.Obj].value.foreach(x => ${add(q"${TermName("x")}")})
              """)
              dataR :+= q"$vr read o"

            // Mandatory field (under own field)
            case Some(key) =>
              dataW :+= ((_:TT) apply q"(($key, $vw write e.$pn))")
              dataR :+= q"$vr read m($key)"

            // Optional field
            case None => keyLookup.get(kn + "_?") match {
              case Some(key) =>
                val o = optionalJsonObjectFieldHelper(pt)
                val e = init valDef o.createEmpty
                val p = q"e.$pn"
                val w = q"(($key, $vw write $p))"
                dataW :+= ((set: TT) => q"if (!${o isEmpty p}) ${set(w)}")
                dataR :+= q"m.get($key).fold($e)($vr.read)"

              case None => fail(s"Key missing for field: $kn")
            }
          }
      }

    val writeToObj: Tree = {
      def applyWrites(tt: TT): Tree =
        q"..${dataW map (_(tt))}"

      gdNT match {
        case None =>
          val s: TT = v => q"vs :+= $v"
          q"""
            var vs = Vector.empty[(String, Js.Value)]
            ${applyWrites(s)}
            json write Js.Obj(vs: _*)
          """

        case Some((gn, gt)) =>
          val gw = summonW(init, gt)
          val s: TT = v => q"vs = $v +: vs"
          // We know we have a Vector by looking at GenericDataMacros. Unit tests will catch change.
          q"""
            var vs = $gw.write(e.$gn).asInstanceOf[Js.Obj].value.asInstanceOf[Vector[(String, Js.Value)]]
            ${applyWrites(s)}
            json write Js.Obj(vs: _*)
          """
      }
    }

    val (readFn: Tree, writeFn: Tree) =
      if (hasId) {
        val f1 = params.head
        val (f1n, f1t) = nameAndType(T, f1)
        val (idByte, idInteger, idMake) = implicitIdFns(f1t)
        val writeFn = writeIdAnd(T)(f1n, idByte, q"Some($idInteger)")(writeToObj)
        (q"$apply($idMake, ..$dataR)", writeFn)
      } else
        (q"$apply(..$dataR)", writeDataOnly(T)(writeToObj))

    val impl =
      q"""
        ..$init
        new DbCodec[$T](
          (b, ii, d) => {
            val o = EventDbMacroImpls readPickledObject d
            val m = o.value.toMap
            $getDataId
            $readFn
          },
          $writeFn)
      """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec[T]](impl)
  }

  case class OptionalJsonObjectFieldHelper(createEmpty: Tree, isEmpty: Tree => Tree = t => q"$t.isEmpty")
  def optionalJsonObjectFieldHelper(t: Type, dealiased: Boolean = false): OptionalJsonObjectFieldHelper = {
    val s = t.toString
    if (s startsWith "Option[")
      OptionalJsonObjectFieldHelper(q"None: $t")
    else if (s startsWith "Set[")
      OptionalJsonObjectFieldHelper(q"Set.empty: $t")
    else if (s startsWith "scala.collection.immutable.Vector[")
      OptionalJsonObjectFieldHelper(q"Vector.empty: $t")
    else if (s startsWith "nyaya.util.Multimap[")
      OptionalJsonObjectFieldHelper(q"nyaya.util.Multimap.empty: $t")
    else if (s matches "^shipreq.webapp.base.text.Text..+.OptionalText")
      OptionalJsonObjectFieldHelper(q"Vector.empty: $t")
    else if (s endsWith "VectorTree.ParentLocation")
      OptionalJsonObjectFieldHelper(q"shipreq.base.util.VectorTree.ParentLocation.Empty: $t")
    else if (!dealiased)
      optionalJsonObjectFieldHelper(t.dealias, true)
    else
      fail(s"EventDbMacros doesn't know how create an empty: $s")
  }

  def quietRegistry[R: c.WeakTypeTag, W <: R: c.WeakTypeTag](typeIds: c.Expr[R => Short]): c.Expr[DbCodec.Registry[R, W]] = implRegistry[R, W](false)(typeIds)
  def debugRegistry[R: c.WeakTypeTag, W <: R: c.WeakTypeTag](typeIds: c.Expr[R => Short]): c.Expr[DbCodec.Registry[R, W]] = implRegistry[R, W](true)(typeIds)
  def  implRegistry[R: c.WeakTypeTag, W <: R: c.WeakTypeTag](debug: Boolean)(typeIds: c.Expr[R => Short]): c.Expr[DbCodec.Registry[R, W]] = {
    val R         = weakTypeOf[R]
    val W         = weakTypeOf[W]
    val types     = findConcreteAdtTypesNE(R, LeavesOnly)
    val dbCodec_  = typeOf[DbCodec[_]]
    val dbCodecR  = appliedType(dbCodec_, R)
    val dbCodecW  = appliedType(dbCodec_, W)
    val init      = Init()
    var remaining = types
    var rCases    = Vector.empty[CaseDef]
    var wCases    = Vector.empty[CaseDef]
    var idsSeen   = Set.empty[Short]

    readMacroArg_tToLitFn(typeIds) foreach {
      case (Right(t), idLit@ Literal(Constant(id: Short))) =>
        if (idsSeen contains id)
          fail(s"Duplicate ID: $t => $id")
        idsSeen += id
        val (matched, unmatched) = remaining.partition(_ <:< t)
        if (matched.size != 1)
          fail(s"Expected $t to match 1 type. Got: $matched")
        remaining   = unmatched
        val t2      = matched.head
        val codecT  = needInferImplicit(appliedType(dbCodec_, t2))
        def codecR  = init valDef q"$codecT.asInstanceOf[$dbCodecR]"
        def codecW  = init valDef q"$codecT.asInstanceOf[$dbCodecW]"
        rCases    :+= cq"$idLit => $codecR"
        if (t <:< W) {
          val wpair = init valDef q"($idLit: Short, $codecW)"
          wCases  :+= cq"_: $t => $wpair"
        }

      case e => fail(s"Unsupported case: ${showRaw(e)}")
    }

    val impl = q"""
      ..$init
      new DbCodec.Registry[$R, $W](i => (i: @scala.annotation.switch) match {case ..$rCases}, {case ..$wCases})
    """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[DbCodec.Registry[R, W]](impl)
  }

  /** unpacks a `ii: Option[Int]` (which is the data_id field) into `i: Int` */
  private def getDataId = q"""def i: Int = ii getOrElse sys.error("data_id is unexpectedly NULL")"""
}
