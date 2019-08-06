package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.{Callback, Reusable, ~=>}
import shipreq.webapp.base.lib.KeyboardTheme

final case class NewEditorArgs(abort           : Option[Callback],
                               commit          : Option[Callback],
                               commitVerb      : String,
                               extraKbShortcuts: KeyboardTheme.Shortcuts) {

  val commitFn: Option[Any ~=> Callback] =
    commit.map(c => Reusable.fn(_ => c))
}

object NewEditorArgs {

  val empty: NewEditorArgs =
    apply(None, None, "", KeyboardTheme.Shortcuts.empty)

  def basic(abort : Option[Callback],
            commit: Option[Callback]): NewEditorArgs =
    apply(
      abort            = abort,
      commit           = commit,
      commitVerb       = KeyboardTheme.Instructions.defaultCommitVerb,
      extraKbShortcuts = KeyboardTheme.Shortcuts.empty)
}