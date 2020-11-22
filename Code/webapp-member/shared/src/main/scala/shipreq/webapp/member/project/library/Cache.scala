package shipreq.webapp.member.project.library

import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.EventOrd

trait Cache {

  /** Safety checks are performed before calling this.
   *
   * @param ord Unless latest is empty, ord > 0 && < latest
   */
  def apply(ord: EventOrd): Option[Project]

  def milestoneIterator(): Iterator[Project]

  def update(latest: Project): Cache
}

object Cache {

  object Disabled extends Cache {
    override def apply(ord: EventOrd)    = None
    override def milestoneIterator()     = Iterator.empty
    override def update(latest: Project) = new Instance(latest)

    final class Instance(latest: Project) extends Cache {

      override def apply(ord: EventOrd) = Some {
        val events = latest.history.events.take(ord.value)
        Project.empty.updateOrThrow(events)
      }

      override def milestoneIterator() =
        Iterator.empty

      override def update(latest: Project) =
        new Instance(latest)
    }
  }
}