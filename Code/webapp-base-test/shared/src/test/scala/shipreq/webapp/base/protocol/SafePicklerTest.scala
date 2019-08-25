package shipreq.webapp.base.protocol

import japgolly.univeq.UnivEq
import scalaz.-\/
import sourcecode.Line
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.test.BinaryTestUtil._

object SafePicklerTest extends TestSuite {
  import SafePickler.DecodingFailure

  private object Internals {

    val v11 = Version.fromInts(1, 1)

    sealed trait Data
    object Data {
      case object O extends Data
      final case class A(i: Int) extends Data
      final case class B(i: Int) extends Data
      implicit def univEq: UnivEq[Data] = UnivEq.derive
    }

    import boopickle.DefaultBasic._
    import Data._

    implicit val picklerA: Pickler[A] =
      transformPickler(A.apply)(_.i)

    implicit val picklerB: Pickler[B] =
      transformPickler(B.apply)(_.i)

     val picklerv10: Pickler[Data] =
      new Pickler[Data] {
        override def pickle(a: Data)(implicit state: PickleState): Unit =
          (a: Any) match {
            case O    => state.enc.writeByte(0)
            case b: A => state.enc.writeByte(1); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Data =
          state.dec.readByte match {
            case 0 => O
            case 1 => state.unpickle[A]
          }
      }

     val picklerv11: Pickler[Data] =
      new Pickler[Data] {
        override def pickle(a: Data)(implicit state: PickleState): Unit =
          a match {
            case O    => state.enc.writeByte(0)
            case b: A => state.enc.writeByte(1); state.pickle(b)
            case b: B => state.enc.writeByte(2); state.pickle(b) // added in v1.1
          }
        override def unpickle(implicit state: UnpickleState): Data =
          state.dec.readByte match {
            case 0 => O
            case 1 => state.unpickle[A]
            case 2 => state.unpickle[B] // added in v1.1
          }
      }

    val safePicklerv10: SafePickler[Data] =
      picklerv10.asV10.withMagicNumbers(123, 456)

    val safePicklerv11: SafePickler[Data] =
      picklerv11.asVersion(v11).withMagicNumbers(123, 456)

    def modBin(b: BinaryData, f: Array[Byte] => Array[Byte]): BinaryData =
      BinaryData.unsafeFromArray(f(b.toNewArray))

    def assertDecodeFailure(p: SafePickler[_], bin: BinaryData)(pf: PartialFunction[DecodingFailure, Unit])
                           (implicit l: Line): Unit =
      p.decode(bin) match {
        case -\/(x) if pf.isDefinedAt(x) => ()
        case x                           => fail("Got: " + x)
      }
  }

  override def tests = Tests {
    import Internals._

    "w>r (ok)" - {
      val data = Data.A(123)
      val bin = safePicklerv11.encode(data)
      assertDecodeOk(safePicklerv10)(bin, data)
    }

    "w>r (ko)" - {
      val data = Data.B(123)
      val bin = safePicklerv11.encode(data)
      assertDecodeFailure(safePicklerv10, bin) {
        case DecodingFailure.ExceptionOccurred(_, Some(`v11`)) =>
      }
    }

    "w<r" - {
      val data = Data.A(123)
      val bin = safePicklerv10.encode(data)
      assertDecodeOk(safePicklerv11)(bin, data)
    }

    'badHeader - {
      val bin = modBin(safePicklerv11.encode(Data.O), 9.toByte +: _)
      val expect = safePicklerv11.header.get
      assertDecodeFailure(safePicklerv11, bin) {
        case DecodingFailure.MagicNumberMismatch(_, `expect`, None) =>
      }
    }

    'badFooter - {
      val bin = modBin(safePicklerv11.encode(Data.O), _.dropRight(1) :+ 9.toByte)
      val expect = safePicklerv11.footer.get
      assertDecodeFailure(safePicklerv11, bin) {
        case DecodingFailure.MagicNumberMismatch(_, `expect`, Some(`v11`)) =>
      }
    }

  }
}
