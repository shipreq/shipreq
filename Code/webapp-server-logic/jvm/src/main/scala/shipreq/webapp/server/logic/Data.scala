package shipreq.webapp.server.logic

import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.Project

final case class ProjectId(value: Long) extends TaggedLong // not AnyVal, it gets boxed

object ProjectId {

  val Extern = ExternalId.scheme[Project, ProjectId](
    ProjectId.apply, _.value,
    "F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")

  final case class AndOwner(id: ProjectId, owner: UserId)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
  * Event ordinal.
  *
  * The order of an event in an event stream.
  */
final case class EventOrd(value: Int) { // not AnyVal, it gets boxed
  def succ = EventOrd(value + 1)
}

object EventOrd {
  implicit val ordering: Ordering[EventOrd] =
    Ordering.by(_.value)
}