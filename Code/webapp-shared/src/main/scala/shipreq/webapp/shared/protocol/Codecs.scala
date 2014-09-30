package shipreq.webapp.shared.protocol

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
    ReadWriter[T](i => Js.Str(i.value.toString), { case Js.Str(i) => C(i.toLong)})

  def boolCase[T](iso: Boolean <=> T) = {
    val T = "t"
    val F = "f"
    ReadWriter[T](t => if (iso from t) Js.Str(T) else Js.Str(F), {
      case Js.Str(T) => iso to true
      case Js.Str(F) => iso to false
    })
  }

  def caseclass6[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, E: Reader : Writer, F: Reader : Writer, Z]
  (y: (A, B, C, D, E, F) => Z, u: Z => Option[(A, B, C, D, E, F)]): ReadWriter[Z] = {
    val r = Tuple6R[A, B, C, D, E, F].read
    val w = Tuple6W[A, B, C, D, E, F].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }
}

object Codecs {
  import Codec._

  implicit def alive = boolCase(Alive)

  implicit def impReq = boolCase(ImplicationRequired)

  implicit def reqTypeMnemonic = tagS(ReqType.Mnemonic.apply)

  implicit def custReqTypeId = tagL(CustReqType.Id.apply)

  implicit def custReqType = caseclass6(CustReqType.apply, CustReqType.unapply)
}
