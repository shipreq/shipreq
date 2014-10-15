package shipreq.webapp.shared.protocol

import shipreq.base.util.BiMap

import scalaz.Isomorphism.<=>
import upickle._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.shared.data._

private[protocol] object Codec {
  //  private def tagS[T <: TaggedString](implicit C: TaggedTypeCtor[T]) =
  //    ReadWriter[T](i => Js.Str(i.value), { case Js.Str(i) => C(i)})
  //  private def tagL[T <: TaggedLong](implicit C: TaggedTypeCtor[T]) =
  //    ReadWriter[T](i => Js.Str(i.value.toString), { case Js.Str(i) => C(i.toLong)})

  def tagS[T <: TaggedString](C: String => T) =
    ReadWriter[T](i => Js.Str(i.value), { case Js.Str(i) => C(i)})

  def tagL[T <: TaggedLong](C: Long => T) =
//    import java.lang.{Long => L}
//    import java.lang.Character.{MAX_RADIX => R} // TODO patch scala.js
//    ReadWriter[T](i => Js.Str(L.toString(i, R)), { case Js.Str(i) => C(L.parseLong(i, R)) })
    ReadWriter[T](i => Js.Str(i.value.toString), { case Js.Str(i) => C(i.toLong)})

  def boolCase[T](iso: Boolean <=> T) = {
    val T = "t"
    val F = "f"
    ReadWriter[T](t => if (iso from t) Js.Str(T) else Js.Str(F), {
      case Js.Str(T) => iso to true
      case Js.Str(F) => iso to false
    })
  }

  // UNSAFE. Make sure tests using exhaustive pattern matching to cover this hierarchy
  def enum[T](ts: T*) = {
    val table = BiMap(ts.zipWithIndex.map(p => p._1 -> ('0' + p._2).toChar.toString).toMap)
    ReadWriter[T](t => Js.Str(table.ab(t)), {
      case Js.Str(k) if table.ba.contains(k) => table.ba(k)
    })
  }

  def caseclass1[A, Z](y: A => Z, u: Z => Option[A])(implicit RA: Reader[A], WA: Writer[A]) =
    ReadWriter[Z](z => WA write u(z).get, RA.read andThen y)

  def caseclass2[A: Reader : Writer, B: Reader : Writer, Z]
  (y: (A, B) => Z, u: Z => Option[(A, B)]): ReadWriter[Z] = {
    val r = Tuple2R[A, B].read
    val w = Tuple2W[A, B].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def caseclass3[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, Z]
  (y: (A, B, C) => Z, u: Z => Option[(A, B, C)]): ReadWriter[Z] = {
    val r = Tuple3R[A, B, C].read
    val w = Tuple3W[A, B, C].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def caseclass4[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, Z]
  (y: (A, B, C, D) => Z, u: Z => Option[(A, B, C, D)]): ReadWriter[Z] = {
    val r = Tuple4R[A, B, C, D].read
    val w = Tuple4W[A, B, C, D].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }
  
  def caseclass5[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, E: Reader : Writer, Z]
  (y: (A, B, C, D, E) => Z, u: Z => Option[(A, B, C, D, E)]): ReadWriter[Z] = {
    val r = Tuple5R[A, B, C, D, E].read
    val w = Tuple5W[A, B, C, D, E].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def caseclass6[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, E: Reader : Writer, F: Reader : Writer, Z]
  (y: (A, B, C, D, E, F) => Z, u: Z => Option[(A, B, C, D, E, F)]): ReadWriter[Z] = {
    val r = Tuple6R[A, B, C, D, E, F].read
    val w = Tuple6W[A, B, C, D, E, F].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def remoteRoutine[R <: Routine.Desc](d: R) = ReadWriter[d.Remote](
    r => Js.Str(r.n),
    {case Js.Str(n) => Routine.Remote(n, d) })

  implicit def deletionAction = enum[DeletionAction](DeletionAction.values: _*)

  implicit def crudable[C <: Crudable](implicit WI: Writer[C#Id], RI: Reader[C#Id], WV: Writer[C#V], RV: Reader[C#V]): ReadWriter[CrudAction[C]] =
    ReadWriter[CrudAction[C]]({
      case CrudAction.Create(v)    => Js.Arr(WV write v)
      case CrudAction.Update(i, v) => Js.Arr(WI write i, WV write v)
      case CrudAction.Delete(i, a) => Js.Arr(WI write i, deletionAction write a, Js.Arr())
    }, {
      case Js.Arr(v)       => CrudAction.Create(RV read v)
      case Js.Arr(i, v)    => CrudAction.Update(RI read i, RV read v)
      case Js.Arr(i, a, _) => CrudAction.Delete(RI read i, deletionAction read a)
    })

}

import Codec._

// =====================================================================================================================
object DataCodecs {

  implicit def alive = boolCase(Alive)

  implicit def impReq = boolCase(ImplicationRequired)

  implicit def reqTypeMnemonic = tagS(ReqType.Mnemonic.apply)

  implicit def customReqTypeId = tagL(CustomReqType.Id.apply)

  implicit def customReqType = caseclass6(CustomReqType.apply, CustomReqType.unapply)

}

// =====================================================================================================================
object RoutineGroupCodecs {
  import Routines._


//  implicit def customReqTypeCrud = crudable[CustomReqTypeCrud]

//  val x = implicitly[Reader[CustomReqTypeCrud.Desc.Remote]]
  implicit def routinesForCfgReqType = caseclass2(ForCfgReqType.apply, ForCfgReqType.unapply)
//  implicit def routinesForCfgReqTypeR: Reader[ForCfgReqType.type] = implicitly
//  implicit def routinesForCfgReqTypeW: Writer[ForCfgReqType.type] = implicitly
}

// =====================================================================================================================
object DeltaCodecs {
  import shipreq.webapp.shared.data.delta._

  implicit def rev = tagL(Rev.apply)

  implicit def partitions = enum[Partition](Partition.CustomReqTypes)

  implicit def remoteDeltaGW = Writer[RemoteDeltaG](r => {
    import r.p.{wd, wp}
    val dp = r.forceDeltaP[r.p.type](r.p)
    val a = partitions write r.p
    val b = rev write r.from
    val c = rev write r.to
    val d = Js.Arr(dp.del.map(wd.write): _*)
    val e = Js.Arr(dp.upd.map(wp.write): _*)
    Js.Arr(a, b, c, d, e)
  })

  implicit def remoteDeltaGR = Reader[RemoteDeltaG]({
    case Js.Arr(a, b, c, Js.Arr(d@_*), Js.Arr(e@_*)) =>
      val p = partitions read a
      val f = rev read b
      val t = rev read c
      val x = d.map(p.rd.read).toList
      val y = e.map(p.rp.read).toList
      RemoteDeltaG(p, f, t)(x, y)
  })

}