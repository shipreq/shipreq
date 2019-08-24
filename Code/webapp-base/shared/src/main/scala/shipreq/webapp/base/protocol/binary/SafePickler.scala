package shipreq.webapp.base.protocol.binary

import boopickle.{PickleState, Pickler, UnpickleState}
import japgolly.univeq._
import java.nio.charset.CharacterCodingException
import scala.util.control.NonFatal
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.BinaryData
import shipreq.webapp.base.protocol.Version

/** Binary codec (pickler). Differs from out-of-the-box [[Pickler]] in the following ways:
  *
  * - decoding is pure: an error value is returned on failure
  * - supports magic numbers at header and footer as a partial message integrity check
  * - supports protocol versioning and evolution
  */
final case class SafePickler[A](header : Option[MagicNumber],
                                footer : Option[MagicNumber],
                                version: Version,
                                body   : Pickler[A]) {

  import boopickle.{PickleImpl, UnpickleImpl}
  import SafePickler._

  // def i1 = "0x%08X".format(new util.Random().nextInt); def i2 = s"$i1, $i1"; def i4 = s"$i2 $i2"
  def withMagicNumbers(header: Int, footer: Int): SafePickler[A] =
    copy(Some(MagicNumber(header)), Some(MagicNumber(footer)))

  private val picklerHeader = header.map(picklerMagicNumber)
  private val picklerFooter = footer.map(picklerMagicNumber)

  private val picklerCombined: Pickler[A] =
    new Pickler[A] {

      override def pickle(a: A)(implicit state: PickleState): Unit = {
        picklerHeader.foreach(_.pickle(()))
        picklerVersion.pickle(version)
        body.pickle(a)
        picklerFooter.foreach(_.pickle(()))
      }

      override def unpickle(implicit state: UnpickleState): A = {
        picklerHeader.foreach(_.unpickle)
        val v = picklerVersion.unpickle
        if (v.major !=* version.major)
          throw DecodingFailure.UnsupportedMajorVer(actual = v, supported = version.major)
        try {
          val a = body.unpickle
          picklerFooter.foreach(_.unpickle)
          a
        } catch {
          case e: Throwable => throw new VerAndErr(v, e)
        }
      }
    }

  def encode(a: A): BinaryData = {
    val bb = PickleImpl.intoBytes(a)(implicitly, picklerCombined)
    BinaryData.unsafeFromByteBuffer(bb)
  }

  def decode(bin: BinaryData): SafePickler.Result[A] = {
    try {
      val a = UnpickleImpl(picklerCombined).fromBytes(bin.unsafeByteBuffer)
      \/-(a)
    } catch {
      case e: VerAndErr => DecodingFailure.fromException(e.err, Some(e.ver))
      case e: Throwable => DecodingFailure.fromException(e, None)
    }
  }
}

object SafePickler {

  type Result[+A] = DecodingFailure \/ A

  sealed trait DecodingFailure {
    val dataVer: Option[Version]
  }

  object DecodingFailure {

    final case class UnsupportedMajorVer(actual   : Version,
                                         supported: Version.Major) extends RuntimeException with DecodingFailure {
      override val dataVer = Some(actual)
    }

    final case class MagicNumberMismatch(actual  : Int,
                                         expected: MagicNumber,
                                         dataVer : Option[Version]) extends RuntimeException with DecodingFailure

    final case class InvalidVersion(major: Int,
                                    minor: Int) extends RuntimeException with DecodingFailure {
      override val dataVer = None
    }

    final case class DecoderException(exception: Throwable,
                                      dataVer  : Option[Version]) extends DecodingFailure

    final case class UnknownException(exception: Throwable,
                                      dataVer  : Option[Version]) extends DecodingFailure

    def fromException(err: Throwable, dataVer: Option[Version]): Result[Nothing] =
      err match {
        case MagicNumberMismatch(a, b, None) => -\/(MagicNumberMismatch(a, b, dataVer))

        case e: DecodingFailure => -\/(e)

        case e @ (_: IllegalArgumentException
                | _: IndexOutOfBoundsException
                | _: CharacterCodingException
                | _: StackOverflowError
          ) => -\/(DecodingFailure.DecoderException(e, dataVer))

        case NonFatal(e) =>
          -\/(DecodingFailure.UnknownException(e, dataVer))

        case e => throw e
      }
  }

  private[SafePickler] final class VerAndErr(val ver: Version, val err: Throwable) extends RuntimeException

  private[SafePickler] val picklerVersion: Pickler[Version] =
    new Pickler[Version] {
      import boopickle.DefaultBasic._

      override def pickle(a: Version)(implicit state: PickleState): Unit = {
        state.enc.writeInt(a.major.value)
        state.enc.writeInt(a.minor.value)
      }

      override def unpickle(implicit state: UnpickleState): Version = {
        val major = state.dec.readInt
        val minor = state.dec.readInt
        val minOk = major >= 1 && minor >= 0
        val maxOk = major <= 4 && minor <= 100
        if (minOk && maxOk)
          Version.fromInts(major, minor)
        else
          throw DecodingFailure.InvalidVersion(major, minor)
      }
    }

  private[SafePickler] def picklerMagicNumber(real: MagicNumber): Pickler[Unit] =
    new Pickler[Unit] {
      import boopickle.DefaultBasic._

      override def pickle(a: Unit)(implicit state: PickleState): Unit = {
        state.enc.writeRawInt(real.value)
      }

      override def unpickle(implicit state: UnpickleState): Unit = {
        val found = state.dec.readRawInt
        if (found != real.value)
          throw DecodingFailure.MagicNumberMismatch(actual = found, expected = real, dataVer = None)
      }
    }

  object ConstructionHelperImplicits {
    implicit class SafePickler_PicklerExt[A](private val self: Pickler[A]) extends AnyVal {
      def asVersion(v: Version): SafePickler[A] = SafePickler(None, None, v, self)
      def asV10 = asVersion(Version.v10)
    }

  }
}

final case class MagicNumber(value: Int)