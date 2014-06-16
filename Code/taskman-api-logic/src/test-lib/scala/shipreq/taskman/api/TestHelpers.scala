package shipreq.taskman.api

import org.scalacheck.{Gen, Arbitrary}
import scala.reflect.runtime.{universe => ru}
import shipreq.base.util.TaggedTypes.{TaggedTypeCtor, TaggedType}
import shipreq.taskman.api.{MsgType => T}
import shipreq.taskman.api.{Msg => M}

object TestHelpers {

  def sealedDescendants[Root: ru.TypeTag]: Option[Set[ru.Symbol]] = {
    import ru._
    val symbol = typeOf[Root].typeSymbol
    val internal = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    if (internal.isSealed)
      Some(internal.sealedDescendants.map(_.asInstanceOf[Symbol]) - symbol)
    else None
  }

  class SealedDescendants[Root : ru.TypeTag] {

    val classes_ : Set[Class[_]] = {
      val m = ru.runtimeMirror(getClass.getClassLoader)
      sealedDescendants[Root].get.toList.map(s => m.runtimeClass(s.asClass)).toSet
    }

    val classes: Set[Class[_ <: Root]] =
      classes_.asInstanceOf[Set[Class[_ <: Root]]]

    val shortNames: List[String] =
      classes_.toList.map(_.getSimpleName)

    val fullNames: Set[String] =
      classes_.map(_.getCanonicalName)

    def size = fullNames.size
  }

  val allMsgs     = new SealedDescendants[Msg]
  val allMsgTypes = new SealedDescendants[MsgType]

  // ===================================================================================================================

  import Arbitrary._

//  def arbTaggedString[T <: TaggedType[String]]: Arbitrary[T] =
//    Arbitrary {      arbitrary[String].map(_.tag[T])    }
  //  implicit def arbEmail: Arbitrary[EmailAddr] = arbTaggedString

//  implicit def arbTagged[U, T <: TaggedType[U]](implicit U: Arbitrary[U], T: TaggedTypeCtor[U, T]): Arbitrary[T] =
//    Arbitrary { U.arbitrary.map(T.apply)}
//  implicit def arbTagged[U, T](implicit U: Arbitrary[U], T: TaggedTypeCtor[U, T]): Arbitrary[T] =
//    Arbitrary { U.arbitrary.map(T.apply)}

//  implicit def arbEmail: Arbitrary[EmailAddr] = arbTagged[String, EmailAddr]
//  def genEmail: Gen[EmailAddr] = arbEmail.arbitrary

  def genEmail: Gen[EmailAddr] = arbitrary[String].map(EmailAddr.apply)
  def genUserId: Gen[UserId] = arbitrary[Long].map(UserId.apply)
  def genUserIdO: Gen[Option[UserId]] = Gen.option(genUserId)

  implicit def arbMsg: Arbitrary[Msg] =
    Arbitrary { Gen.oneOf(allMsgs.classes.toSeq) flatMap genMsg }

  def genMsg(c: Class[_ <: Msg]): Gen[Msg] = genMsg(MsgType.lookup(c))

  def genMsg(m: MsgType): Gen[Msg] = m match {

    case T.RegistrationRequested =>
      for(email <- genEmail; url <- arbitrary[String])
      yield M.RegistrationRequested(email, url)

    case T.ReRegistrationAttempted =>
      for(email <- genEmail)
      yield M.ReRegistrationAttempted(email)

    case T.RegistrationCompleted =>
      for (userId <- genUserId) yield
        M.RegistrationCompleted(userId)

    case T.UserUpdated =>
      for (userId <- genUserId) yield
        M.UserUpdated(userId)

    case T.PasswordResetRequested =>
      for(email <- genEmail; url <- arbitrary[String])
      yield M.PasswordResetRequested(email, url)

    case T.LandingPageHit =>
      for {
        email <- genEmail
        name <- arbitrary[String]
        msg <- arbitrary[Option[String]]
        newsletter <- arbitrary[Boolean]
      } yield
        M.LandingPageHit(email, name, msg, newsletter)

    case T.WebappErrorOccurred =>
      for {
        usr <- genUserIdO
        url <- arbitrary[Option[String]]
        msg <- arbitrary[String]
      } yield
        M.WebappErrorOccurred(usr, url, msg)

    case T.DummyMsg =>
      for {
        desc             <- arbitrary[String]
        async            <- arbitrary[Boolean]
        processingTimeMs <- arbitrary[Long]
        retryCount       <- arbitrary[Short]
        retryDelaySec    <- arbitrary[Int]
        failureMsg       <- arbitrary[Option[String]]
      } yield
        M.DummyMsg(desc, async, processingTimeMs, retryCount, retryDelaySec, failureMsg)

    case T.SendDiagEmail =>
      for {
        email   <- genEmail
        subject <- arbitrary[String]
        body    <- arbitrary[String]
      } yield
        M.SendDiagEmail(email, subject, body)

    case T.SyncToMailingList =>
      for (sql <- arbitrary[Option[String]]) yield
        M.SyncToMailingList(sql)

  }

//  def genMsgOfEachType: Gen[List[Msg]] =
//    Gen.sequence[List, Msg](taskDefClasses.toList map genMsg)
}
