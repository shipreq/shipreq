package shipreq.taskman.api

import scala.reflect.runtime.{universe => ru}
import org.scalacheck.{Gen, Arbitrary}
import shipreq.taskman.api.Types._
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

//  def arbTaggedString[T <: TypeTag[String]]: Arbitrary[String @@ T] =
//    Arbitrary {      arbitrary[String].map(_.tag[T])    }
  //  implicit def arbEmail: Arbitrary[EmailAddr] = arbTaggedString

  def genEmail: Gen[EmailAddr] = arbitrary[String].map(_.tag)
  def genUserId: Gen[UserId] = arbitrary[Long].map(_.tag)

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

    case T.DummyMsg =>
      for {
        desc             <- arbitrary[String]
        processingTimeMs <- arbitrary[Long]
        retryCount       <- arbitrary[Short]
        failureMsg       <- arbitrary[Option[String]]
      } yield
        M.DummyMsg(desc, processingTimeMs, retryCount, failureMsg)
  }

//  def genMsgOfEachType: Gen[List[Msg]] =
//    Gen.sequence[List, Msg](taskDefClasses.toList map genMsg)
}
