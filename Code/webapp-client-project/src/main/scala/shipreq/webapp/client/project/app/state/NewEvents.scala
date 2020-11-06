package shipreq.webapp.client.project.app.state

import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{EventSeqSummary, VerifiedEvent}

final case class NewEvents(events: VerifiedEvent.Seq, project: Project) {
  def isEmpty            = events.isEmpty
  val summary            = EventSeqSummary.fromVerifiedEvents(events)
  val summaryWithProject = summary.withProject(project)
}

object NewEvents {

  def empty: NewEvents =
    apply(VerifiedEvent.Seq.empty, Project.empty)
}