package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.{Callback, Reusable, ~=>}
import shipreq.webapp.base.feature.EditControlsFeature

final case class NewEditorArgs(abort        : Option[Callback],
                               autoFocus    : Boolean,
                               commit       : Option[Callback],
                               commitVerb   : String,
                               extraControls: EditControlsFeature.ExtraControls) {

  val commitFn: Option[Any ~=> Callback] =
    commit.map(c => Reusable.fn(_ => c))
}

object NewEditorArgs {

  val empty: NewEditorArgs =
    apply(
      abort         = None,
      autoFocus     = true,
      commit        = None,
      commitVerb    = "",
      extraControls = EditControlsFeature.ExtraControls.empty)

  def basic(abort : Option[Callback],
            commit: Option[Callback]): NewEditorArgs =
    apply(
      abort         = abort,
      autoFocus     = true,
      commit        = commit,
      commitVerb    = EditControlsFeature.defaultCommitVerb,
      extraControls = EditControlsFeature.ExtraControls.empty)
}