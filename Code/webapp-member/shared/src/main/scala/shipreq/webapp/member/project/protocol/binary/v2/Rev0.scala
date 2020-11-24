package shipreq.webapp.member.project.protocol.binary.v2

import shipreq.webapp.base.protocol.binary.v1.BaseData.pickleDisj
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._

/** v2.0 */
object Rev0 {
  import boopickle.DefaultBasic._
  import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData2._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev6._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev7._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev7.SavedViewPicklers._

  implicit lazy val picklerProjectEvents: Pickler[ProjectEvents] =
    transformPickler(ProjectEvents.apply)(_.events)

  implicit lazy val picklerProject: Pickler[Project] =
    new Pickler[Project] {
      override def pickle(a: Project)(implicit state: PickleState): Unit = {
        state.pickle(a.name)
        state.pickle(a.config)
        state.pickle(a.content)
        state.pickle(a.manualIssues)
        state.pickle(a.savedViews)
        state.pickle(a.history)
        state.pickle(a.idCeilings)
      }
      override def unpickle(implicit state: UnpickleState): Project = {
        val name          = state.unpickle[Project.Name]
        val config        = state.unpickle[ProjectConfig]
        val content       = state.unpickle[ProjectContent]
        val manualIssues  = state.unpickle[ManualIssues]
        val savedViews    = state.unpickle[savedview.SavedViews.Optional]
        val history       = state.unpickle[ProjectEvents]
        val idCeilings    = state.unpickle[IdCeilings]
        Project(name, config, content, manualIssues, savedViews, history, idCeilings)
      }
    }

  implicit lazy val picklerProjectOrEvents: Pickler[Project \/ VerifiedEvent.Seq] =
    pickleDisj
}
