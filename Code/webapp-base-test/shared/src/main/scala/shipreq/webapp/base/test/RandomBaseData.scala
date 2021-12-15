package shipreq.webapp.base.test

import monocle.Lens
import nyaya.gen.Gen
import shipreq.base.util._
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.{Obfuscated, PreProcessor}

object RandomBaseData {
  import RandomDataSettings._

  @tailrec def dropHead[A](v: ArraySeq[A])(f: A => Boolean): ArraySeq[A] =
    if (v.nonEmpty && f(v.head))
      dropHead(v.tail)(f)
    else
      v

  @tailrec def dropLast[A](v: ArraySeq[A])(f: A => Boolean): ArraySeq[A] =
    if (v.nonEmpty && f(v.last))
      dropLast(v.init)(f)
    else
      v

  /*
  def genmodL[A, B](l: Lens[A, B])(g: B => Gen[B])(a: A): Gen[A] =
    g(l get a) map (l.set(_)(a))
  */

  //    val trimLeftR = "^\\s+".r
  //    def trimLeft(s: String) = trimLeftR.replaceAllIn(s, "")
  //    val trimRightR = "\\s+$".r
  //    def trimRight(s: String) = trimRightR.replaceAllIn(s, "")

  private[this] val charsUpper = ('A' to 'Z').toArray
  private[this] val charsLower = ('a' to 'z').toArray
  private[this] val charsAlpha = charsUpper ++ charsLower
  private[this] val charsAlphaSlash = charsAlpha :+ '/'

  val genAlphaSlash = Gen.chooseArray_!(charsAlphaSlash)

  val unicodeChar: Gen[Char] =
    if (disableUnicode)
      Gen.ascii
    else {
      val a = new Array[Char](1)
      val chars = (0 to 65535)
        .iterator
        .map(_.toChar)
        .filter { c =>
          a(0) = c
          PreProcessor.FixChar.multiLine(a, 0)
          a(0) ==* c
        }
        .toVector
      Gen.choose_!(chars)
    }

  val unicodeString: Gen[String] = unicodeChar.string
  val unicodeString1: Gen[String] = unicodeChar.string1

  class CaseInsensitive(val norm: String, val str: String) {
    override def hashCode = norm.##

    override def equals(o: Any) = o match {
      case x: CaseInsensitive => norm == x.norm
      case _ => false
    }
  }

  def CaseInsensitive(s: String): CaseInsensitive =
    new CaseInsensitive(s.toLowerCase, s)

  def someOfWithDups[A, B](as: Seq[A])(f: A => Gen[B]): Gen[Vector[B]] =
    Gen.tryGenChoose(as).fold[Gen[Vector[B]]](Gen pure Vector.empty)(
      _.vector.flatMap(Gen.traverse(_)(f)))

  val shortText1 = unicodeChar.string(1 to WebappConfig.shortTextMaxLength)
  val shortText = unicodeChar.string(0 to WebappConfig.shortTextMaxLength)
  val optionalLargeText = unicodeChar.string(1 to WebappConfig.largeTextMaxLength).option

  def imapToMapLens[K, V] = Lens((_: IMap[K, V]).underlyingMap)(v => _ replaceUnderlying v)

  val enabled =
    Gen.boolean.map(Enabled.when)

  val applicability: Gen[Applicability] =
    Gen.boolean.map(Applicable.when)

  val dir =
    Gen.choose[Direction](Forwards, Backwards)

  val alphaOne =
    Gen.alpha.map(_.toString)

  def obfuscated[A]: Gen[Obfuscated[A]] =
    Gen.alphaNumeric.string(4 to 12).map(Obfuscated.apply[A])

  lazy val username: Gen[Username] = {
    val x = WebappConfig.usernameLength.min - 2
    val y = WebappConfig.usernameLength.max - 2
    for {
      a <- Gen.lower
      b <- Gen.chooseChar('_', 'a' to 'z', '0' to '9').string(x to y)
      c <- Gen.chooseChar('a', 'b' to 'z', '0' to '9')
    } yield Username("" + a + b + c)
  }

  lazy val errorMsg: Gen[ErrorMsg] =
    Gen.ascii.string(1 to 6).map(ErrorMsg.apply)

  lazy val emailAddr: Gen[EmailAddr] =
    Gen.ascii.string(0 to 6).map(EmailAddr.apply)

  lazy val plainTextPassword: Gen[PlainTextPassword] =
    unicodeString.map(PlainTextPassword.apply)

  lazy val personName: Gen[PersonName] =
    unicodeString1.map(PersonName.apply)

  lazy val verificationToken: Gen[VerificationToken] =
    Gen.ascii.string(1 to 6).map(VerificationToken.apply)

  lazy val genHexCharLower: Gen[Char] =
    Gen.chooseChar('a', "bcdef0123456789")

  def projectIdPublic: Gen[ProjectId.Public] =
    obfuscated

  lazy val userIdPublic: Gen[UserId.Public] =
    obfuscated

}
