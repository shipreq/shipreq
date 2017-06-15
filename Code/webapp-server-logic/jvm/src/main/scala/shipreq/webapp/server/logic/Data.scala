package shipreq.webapp.server.logic

final case class ProjectId(value: Long) // extends AnyVal - nope, it gets boxed

final case class EventSeq(value: Int) extends AnyVal {
  def succ = EventSeq(value + 1)
}
