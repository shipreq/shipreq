package shipreq.taskman.api

import scala.reflect.runtime.{universe => ru}
import org.scalacheck.{Gen, Arbitrary}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{TaskType => T}
import shipreq.taskman.api.{TaskDef => D}

object TestHelpers {

  def sealedDescendants[Root: ru.TypeTag]: Option[Set[ru.Symbol]] = {
    import ru._
    val symbol = typeOf[Root].typeSymbol
    val internal = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    if (internal.isSealed)
      Some(internal.sealedDescendants.map(_.asInstanceOf[Symbol]) - symbol)
    else None
  }

  lazy val taskDefClassShortNames: List[String] =
    TestHelpers.sealedDescendants[TaskDef].get.toList.map(_.name.toString)

  lazy val taskDefClassFullNames: Set[String] =
    taskDefClassShortNames.map(TaskDef.getClass.getCanonicalName + _).toSet

  lazy val taskDefClasses: Set[Class[_ <: TaskDef]] =
    taskDefClassFullNames.map(n => Class.forName(n).asSubclass(classOf[TaskDef]))

  import Arbitrary._

//  def arbTaggedString[T <: TypeTag[String]]: Arbitrary[String @@ T] =
//    Arbitrary {      arbitrary[String].map(_.tag[T])    }
  //  implicit def arbEmail: Arbitrary[EmailAddr] = arbTaggedString

  def genEmail: Gen[EmailAddr] = arbitrary[String].map(_.tag)
  def genUserId: Gen[UserId] = arbitrary[Long].map(_.tag)

  implicit def arbTaskDef: Arbitrary[TaskDef] =
    Arbitrary { Gen.oneOf(taskDefClasses.toSeq) flatMap genTaskDef }

  def genTaskDef(c: Class[_ <: TaskDef]): Gen[TaskDef] = genTaskDef(TaskTypes.lookupType(c))

  def genTaskDef(t: TaskType): Gen[TaskDef] = t match {

    case T.RegistrationRequested =>
      for(email <- genEmail; url <- arbitrary[String])
      yield D.RegistrationRequested(email, url)

    case T.ReRegistrationAttempted =>
      for(email <- genEmail; url <- arbitrary[String])
      yield D.ReRegistrationAttempted(email, url)

    case T.RegistrationCompleted =>
      for (userId <- genUserId) yield
        D.RegistrationCompleted(userId)

    case T.PasswordResetRequested =>
      for(email <- genEmail; url <- arbitrary[String])
      yield D.PasswordResetRequested(email, url)

    case T.LandingPageHit =>
      for {
        email <- genEmail
        name <- arbitrary[String]
        msg <- arbitrary[Option[String]]
        newsletter <- arbitrary[Boolean]
      } yield
        D.LandingPageHit(email, name, msg, newsletter)
  }

//  def genTaskDefOfEachType: Gen[List[TaskDef]] =
//    Gen.sequence[List, TaskDef](taskDefClasses.toList map genTaskDef)
}
