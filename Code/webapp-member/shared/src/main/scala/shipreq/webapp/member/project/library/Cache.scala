package shipreq.webapp.member.project.library

import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.EventOrd

trait Cache {

  /** Safety checks are performed before calling this.
   *
   * @param ord Unless latest is empty, 0 < ord < latest
   */
  def apply(ord: EventOrd): Option[Project]

  def storePotentialMilestone(p: Project): Unit

  def iterator(): Iterator[Project]

  final def update(projects: Iterable[Project]): Cache =
    if (projects.isEmpty)
      this
    else
      updateNE(projects)

  final def update(project: Project): Cache =
    updateNE(project :: Nil)

  protected def updateNE(projects: Iterable[Project]): Cache
}

object Cache {

  object Disabled extends Cache {

    override def apply(ord: EventOrd) =
      None

    override def storePotentialMilestone(p: Project): Unit =
      ()

    override def iterator() =
      Iterator.empty

    override protected def updateNE(projects: Iterable[Project]) = {
      val latest = projects.iterator.maxBy(_.ordAsInt)
      new Instance(latest)
    }

    final class Instance(latest: Project) extends Cache {

      override def apply(ord: EventOrd) = Some {
        val events = latest.history.events.take(ord.value)
        Project.empty.updateOrThrow(events)
      }

      override def storePotentialMilestone(p: Project): Unit =
        ()

      override def iterator() =
        Iterator.single(latest)

      override protected def updateNE(projects: Iterable[Project]) = {
        val newLatest = projects.iterator.maxBy(_.ordAsInt)
        if (newLatest > latest)
          new Instance(newLatest)
        else
          this
      }
    }
  }

}