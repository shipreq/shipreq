package shipreq.webapp.base.feature.editcontrols

import japgolly.scalajs.react._
import shipreq.webapp.base.lib.{KeyHandler, KeyHandlers}

final class ExtraControls(val keys: KeyHandlers,
                          val instructions: List[Instructions.Clause]) {

  def ++(e: ExtraControls): ExtraControls =
    new ExtraControls(
      keys ++ e.keys,
      instructions ::: e.instructions,
    )
}

object ExtraControls {
  val empty: ExtraControls =
    new ExtraControls(KeyHandlers.empty, Nil)

  val emptyFn: Any => ExtraControls =
    _ => empty

  def option(criterion: KeyHandler.Criterion, verb: String, action: Option[Callback]): ExtraControls =
    new ExtraControls(
      criterion.handleWhenDefined(action).toKeyHandlers,
      action.map(Instructions.Clause.keyToAction(criterion.desc)(verb, _)).toList)

  def commitAndProgressWhenDefined(actionOption: Option[Callback], verb: String): ExtraControls =
    option(Keys.commitAndProgress, verb, actionOption)
}
