package shipreq.taskman.api

/**
 * A datum that can be sent to the Taskman server and meaningfully processed.
 */
sealed trait Msg

object Msg {
  import Types._

  // ********************************************************************
  // * NOTE: Fields names here need to match the JSON FieldSerializers. *
  // ********************************************************************

  case class RegistrationRequested(email: EmailAddr, verifyEmailUrl: String) extends Msg

  case class RegistrationCompleted(userId: UserId) extends Msg

  case class ReRegistrationAttempted(email: EmailAddr) extends Msg

  case class PasswordResetRequested(email: EmailAddr, resetPasswordUrl: String) extends Msg

  case class LandingPageHit(email: EmailAddr, name: String, msg: Option[String], newsletter: Boolean) extends Msg

  case class DummyMsg(desc: String,
                      processingTimeMs: Long = 0,
                      retryCount: Short = 0,
                      retryDelaySec: Int = 0,
                      failureMsg: Option[String] = None) extends Msg

  // UserChangedPrefs
  // MailChimpBroadcast
}

// =====================================================================================================================

sealed abstract class MsgType(val id: Short, val msgClass: Class[_ <: Msg])

object MsgType {
  case object DummyMsg                extends MsgType(  1, classOf[Msg.DummyMsg])
  case object RegistrationRequested   extends MsgType(100, classOf[Msg.RegistrationRequested])
  case object RegistrationCompleted   extends MsgType(101, classOf[Msg.RegistrationCompleted])
  case object ReRegistrationAttempted extends MsgType(102, classOf[Msg.ReRegistrationAttempted])
  case object PasswordResetRequested  extends MsgType(103, classOf[Msg.PasswordResetRequested])
  case object LandingPageHit          extends MsgType(200, classOf[Msg.LandingPageHit])

  val values = List[MsgType](
    RegistrationRequested
    , RegistrationCompleted
    , ReRegistrationAttempted
    , PasswordResetRequested
    , LandingPageHit
    , DummyMsg
  )

  private[this] val byId: Map[Short, MsgType] = {
    val groups = values.groupBy(_.id)
    val dupGroups = groups.filterNot(_._2.size == 1)
    if (dupGroups.nonEmpty)
      throw new ExceptionInInitializerError(s"${classOf[MsgType].getSimpleName} with duplicate IDs found: $dupGroups")
    groups.mapValues(_.head).toMap
  }

  private[this] val byMsgClass: Map[Class[_ <: Msg], MsgType] = {
    val groups = values.groupBy(_.msgClass)
    val dupGroups = groups.filterNot(_._2.size == 1)
    if (dupGroups.nonEmpty)
      throw new ExceptionInInitializerError(s"${classOf[MsgType].getSimpleName} with duplicate ${classOf[Msg].getSimpleName} classes found: $dupGroups")
    groups.mapValues(_.head).toMap
  }

  assert(byId.size == byMsgClass.size)

  @inline def lookup(id: Short): Option[MsgType] = byId.get(id)
  @inline def lookup_!(id: Short): MsgType = byId(id)
  @inline def lookup(c: Class[_ <: Msg]): MsgType = byMsgClass(c)
  @inline def lookup(d: Msg): MsgType = byMsgClass(d.getClass)
}
