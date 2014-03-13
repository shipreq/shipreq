package shipreq.taskman.api

import Types._

object TaskTypes {
  import java.util.EnumMap
  import shipreq.taskman.api.{TaskType => T}
  import shipreq.taskman.api.{TaskDef => D}

  val AsList: List[TaskType] = TaskType.values.toList

  private[this] val ById: Map[Int, TaskType] = {
    val groups = AsList.groupBy(_.id)
    val dupGroups = groups.filterNot(_._2.size == 1)
    if (dupGroups.nonEmpty)
      throw new ExceptionInInitializerError(s"${classOf[TaskType].getSimpleName} with duplicate IDs found: $dupGroups")
    groups.mapValues(_.head).toMap
  }

  @inline def lookupType(id: Int): Option[TaskType] = ById.get(id)
  @inline def lookupType_!(id: Int): TaskType = ById(id)

  private[this] val TypeToDef: EnumMap[TaskType, Class[_ <: TaskDef]] = {
    def trans(tt: T): Class[_ <: TaskDef] = tt match {
      case T.RegistrationRequested  => classOf[D.RegistrationRequested]
      case T.RegistrationCompleted  => classOf[D.RegistrationCompleted]
      case T.PasswordResetRequested => classOf[D.PasswordResetRequested]
      case T.LandingPageHit         => classOf[D.LandingPageHit]
    }
    val m = new EnumMap[TaskType, Class[_ <: TaskDef]](classOf[TaskType])
    for (tt <- TaskType.values) m.put(tt, trans(tt))
    m
  }

  @inline def lookupTaskDef(t: TaskType): Class[_ <: TaskDef] = TypeToDef.get(t)

  private[this] val DefToType: Map[Class[_ <: TaskDef], TaskType] =
    TaskType.values.map(t => (lookupTaskDef(t) -> t)).toMap

  @inline def lookupType(c: Class[_ <: TaskDef]): TaskType = DefToType(c)
  @inline def lookupType(d: TaskDef): TaskType = DefToType(d.getClass)
}


sealed trait TaskDef
object TaskDef {
  case class RegistrationRequested(email: EmailAddr, url: Option[String]) extends TaskDef
  case class RegistrationCompleted(userId: UserId) extends TaskDef
  case class PasswordResetRequested(email: EmailAddr, url: String) extends TaskDef
  case class LandingPageHit(email: EmailAddr, name: String, msg: Option[String], newsletter: Boolean) extends TaskDef
  // UserChangedPrefs
  // MailChimpBroadcast
}
