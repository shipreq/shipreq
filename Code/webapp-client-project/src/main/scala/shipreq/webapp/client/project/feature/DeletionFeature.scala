package shipreq.webapp.client.project.feature

import shipreq.webapp.base.data.{Project, ReqId}

object DeletionFeature {

  // type DeleteOrRestore = deletion.DeleteOrRestore
  // val  DeleteOrRestore = deletion.DeleteOrRestore
  // val  Delete          = deletion.Delete
  // val  Restore         = deletion.Restore
  import deletion.{Delete, Restore}

  type Data = deletion.DeletionRestorationLogic.Data
  val  Data = deletion.DeletionRestorationLogic.Data

  def deletionData(project: Project, directlySelected: NonEmptySet[ReqId]): Data =
    deletion.DeletionRestorationLogic.forReqs(Delete, project, directlySelected)

  def restorationData(project: Project, directlySelected: NonEmptySet[ReqId]): Data =
    deletion.DeletionRestorationLogic.forReqs(Restore, project, directlySelected)

  val DeletionFormProps = deletion.DeletionForm.Props

  // Might want to add some logic here later to not bother showing the dialog if there are no implied reqs
  // to select.
  // On the other hand, it's probably better UX to always present users with the same Cancel/Restore choice before
  // taking action.
  val RestorationFormProps = deletion.RestorationForm.Props

}
