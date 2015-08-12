package shipreq.webapp.server.db

import org.postgresql.util.PGobject
import scala.reflect.macros.blackbox.Context
import shipreq.base.macros.MacroUtils
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
class DbCodec[E](val read : (Byte, Integer, String) => E,
                 val write: E => (PGobject, Integer, String))

object DbCodec {
  final class MonoId[A](final val byte: Byte, val integer: A => Integer, val make: Integer => A) {
    val bytePG = EventDbMacroImpls makeTypeIdPG byte
  }
  def monoId[A <: TaggedInt](byte: Byte, make: Int => A): MonoId[A] =
    new MonoId(byte, _.value, i => make(i.intValue))

  final case class PolyId[A](bytePG: A => PGobject, integer: A => Integer, make: Byte => Integer => A)
  def  polyId[A]: DbCodec.PolyId[A] = macro EventDbMacroImpls.quietPolyId[A]
  def _polyId[A]: DbCodec.PolyId[A] = macro EventDbMacroImpls.debugPolyId[A]

  class Registry[E](val reader: Short => DbCodec[E], val writer: E => (Short, DbCodec[E]))
  def  registry[E](typeIds: E => Short): DbCodec.Registry[E] = macro EventDbMacroImpls.quietRegistry[E]
  def _registry[E](typeIds: E => Short): DbCodec.Registry[E] = macro EventDbMacroImpls.debugRegistry[E]
}

object EventDbMacros {
  final val noDataIdType: Byte = ' '
  val noDataIdTypePG = EventDbMacroImpls.makeTypeIdPG(noDataIdType)

  def dbCodecIdOnly [E]: DbCodec[E] = macro EventDbMacroImpls.quietDbCodecIdOnly[E]
  def _dbCodecIdOnly[E]: DbCodec[E] = macro EventDbMacroImpls.debugDbCodecIdOnly[E]

  def dbCodecDataOnly [E]: DbCodec[E] = macro EventDbMacroImpls.quietDbCodecDataOnly[E]
  def _dbCodecDataOnly[E]: DbCodec[E] = macro EventDbMacroImpls.debugDbCodecDataOnly[E]

  def dbCodec2 [E]: DbCodec[E] = macro EventDbMacroImpls.quietDbCodec2[E]
  def _dbCodec2[E]: DbCodec[E] = macro EventDbMacroImpls.debugDbCodec2[E]

  def dbCodecIdAnd [E](keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.quietDbCodecIdAnd[E]
  def _dbCodecIdAnd[E](keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.debugDbCodecIdAnd[E]

  def dbCodecIdGdAnd [E](gd: Symbol, keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.quietDbCodecIdGdAnd[E]
  def _dbCodecIdGdAnd[E](gd: Symbol, keys: (Symbol, String)*): DbCodec[E] = macro EventDbMacroImpls.debugDbCodecIdGdAnd[E]
}

// =====================================================================================================================
object EventDbMacroImpls {
  import upickle._

  def makeTypeIdPG(b: Byte): PGobject = {
    val o = new PGobject()
    o.setType("char")
    o.setValue(b.toChar.toString)
    o
  }

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
      implicitMono(t).map(f => (q"$f.bytePG", q"$f integer a", q"$f make i"))
    def tryPoly: Option[IdFns] =
      implicitPoly(t).map(f => (q"$f bytePG a", q"$f integer a", q"$f.make(b)(i)"))
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
      intCases  :+= cq"i: $t2 => $mono integer i"
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

    if (debug) println("\n" + impl + "\n")
    c.Expr[DbCodec.PolyId[T]](impl)
  }

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

    val (idByte, idInteger, idMake) = implicitIdFns(ft)
    val writeFn = writeIdAnd(T)(fn, idByte, idInteger)(Literal(Constant(null)))

    val impl = q"new DbCodec[$T]((b, i, _) => $apply($idMake), $writeFn)"

    if (debug) println("\n" + impl + "\n")
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

    val impl = q"""
      ..$init
      new DbCodec[$T](
        (_, _, d) => $apply($readData),
        e => (noDataIdTypePG, null, $writeData))
    """

    if (debug) println("\n" + impl + "\n")
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
    val writeFn = writeIdAnd(T)(f1n, idByte, idInteger)(writeToJsonStr(f2w, q"e.$f2n"))
    val readData = readFromJsonStr(f2r, q"d")

    val impl = q"""
      ..$init
      new DbCodec[$T]((b, i, d) => $apply($idMake, $readData), $writeFn)
    """

    if (debug) println("\n" + impl + "\n")
    c.Expr[DbCodec[T]](impl)
  }

  type SymStrPair = (scala.Symbol, String)
  def quietDbCodecIdAnd[T: c.WeakTypeTag](keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecIdGdAnd[T](false)(None, keys)
  def debugDbCodecIdAnd[T: c.WeakTypeTag](keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecIdGdAnd[T](true )(None, keys)
  def quietDbCodecIdGdAnd[T: c.WeakTypeTag](gd: c.Expr[scala.Symbol], keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecIdGdAnd[T](false)(Some(gd), keys)
  def debugDbCodecIdGdAnd[T: c.WeakTypeTag](gd: c.Expr[scala.Symbol], keys: c.Expr[SymStrPair]*): c.Expr[DbCodec[T]] = implDbCodecIdGdAnd[T](true )(Some(gd), keys)
  def implDbCodecIdGdAnd[T: c.WeakTypeTag](debug: Boolean)(gdArg: Option[c.Expr[scala.Symbol]], keys: Seq[c.Expr[SymStrPair]]): c.Expr[DbCodec[T]] = {
    val T      = concreteWeakTypeOf[T]
    val apply  = tcApplyFn(T)
    val params = primaryConstructorParams(T).toVector

    if (params.length < 3)
      fail(s"Expected at least 3 fields. Found: $params")

    val keyLookup: Map[String, Literal] =
      keys map (readMacroArg_symbolString(_)) toMap

    if (debug) println(s"KeyLookup: $keyLookup")

    val ps         = params.tail map (nameAndType(T, _))
    val expKeySize = ps.length - gdArg.size
    if (keyLookup.size != expKeySize)
      fail(s"Expected $expKeySize keys, got ${keyLookup.size}.\n  Fields: $ps\n  Keys: $keyLookup")

    val gdNT = gdArg.map { arg =>
      val gd = readMacroArg_symbol(arg)
      ps.find(_._1.toString == gd)
        .getOrElse(fail(s"GenericData field $gd not found. Available: ${ps.map(_._1.toString)}"))
    }

    val init  = Init(importMPickle)
    var dataW = Vector.empty[Tree]
    var dataR = Vector.empty[Tree]

    for ((pn, pt) <- ps)
      gdNT match {
        case Some((gn, gt)) if gn eq pn =>
          val gr = summonR(init, gt)
          dataR :+= q"$gr read o"
        case _ =>
          val kn = pn.decodedName.toString
          val key = keyLookup get kn getOrElse fail(s"Key missing for field: $kn")
          val (vr, vw) = summonRW(init, pt)
          dataW :+= q"(($key, $vw write e.$pn))"
          dataR :+= q"$vr read m($key)"
      }

    val f1 = params.head
    val (f1n, f1t) = nameAndType(T, f1)
    val (idByte, idInteger, idMake) = implicitIdFns(f1t)

    val writeToObj: Tree = gdNT match {
      case None =>
        q"json write Js.Obj(..$dataW)"
      case Some((gn, gt)) =>
        val gw = summonW(init, gt)
        // We know we have a Vector by looking at GenericDataMacros. Unit tests will catch change.
        q"""
          val gvs = $gw.write(e.$gn).asInstanceOf[Js.Obj].value.asInstanceOf[Vector[(String, Js.Value)]]
          val vs = ${dataW.foldLeft[Tree](q"gvs")((q, expr) => q"$q :+ $expr")}
          json write Js.Obj(vs: _*)
        """
    }

    val writeFn = writeIdAnd(T)(f1n, idByte, idInteger)(writeToObj)

    val impl = q"""
      ..$init
      new DbCodec[$T](
        (b, i, d) => {
          val o = EventDbMacroImpls readPickledObject d
          val m = o.value.toMap
          $apply($idMake, ..$dataR)
        },
        $writeFn)
    """

    if (debug) println("\n" + impl + "\n")
    c.Expr[DbCodec[T]](impl)
  }

  def quietRegistry[T: c.WeakTypeTag](typeIds: c.Expr[T => Short]): c.Expr[DbCodec.Registry[T]] = implRegistry[T](false)(typeIds)
  def debugRegistry[T: c.WeakTypeTag](typeIds: c.Expr[T => Short]): c.Expr[DbCodec.Registry[T]] = implRegistry[T](true)(typeIds)
  def implRegistry[T: c.WeakTypeTag](debug: Boolean)(typeIds: c.Expr[T => Short]): c.Expr[DbCodec.Registry[T]] = {
    val T         = weakTypeOf[T]
    val types     = findConcreteTypesNE(T, LeavesOnly).map(t => determineAdtType(T, t))
    val dbCodec_  = typeOf[DbCodec[_]]
    val dbCodecT  = appliedType(dbCodec_, T)
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
        val codecT2 = needInferImplicit(appliedType(dbCodec_, t2))
        val codecT  = init valDef q"$codecT2.asInstanceOf[$dbCodecT]"
        val wpair   = init valDef q"($idLit: Short, $codecT)"
        rCases    :+= cq"$idLit => $codecT"
        wCases    :+= cq"_: $t => $wpair"

      case e => fail(s"Unsupported case: ${showRaw(e)}")
    }

    val impl = q"""
      ..$init
      new DbCodec.Registry[$T](i => (i: @scala.annotation.switch) match {case ..$rCases}, {case ..$wCases})
    """

    if (debug) println("\n" + impl + "\n")
    c.Expr[DbCodec.Registry[T]](impl)
  }
}
