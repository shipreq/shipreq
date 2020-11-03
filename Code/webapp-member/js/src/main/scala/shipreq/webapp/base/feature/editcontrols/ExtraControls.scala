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
  val empty: Reusable[ExtraControls] =
    Reusable.byRef(new ExtraControls(KeyHandlers.empty, Nil))

  val emptyFn: Any => Reusable[ExtraControls] =
    _ => empty

  def option(criterion: KeyHandler.Criterion, verb: String, action: Option[Reusable[Callback]]): Reusable[ExtraControls] =
    Reusable.implicitly((criterion, verb, action)).withValue(
      _option(criterion, verb, action.map(_.value)))

  private def _option(criterion: KeyHandler.Criterion, verb: String, action: Option[Callback]): ExtraControls =
    new ExtraControls(
      criterion.handleWhenDefined(action).toKeyHandlers,
      action.map(Instructions.Clause.keyToAction(criterion.desc)(verb, _)).toList)

  def commitAndProgress(action: Reusable[Callback], verb: String): Reusable[ExtraControls] = {
    val criterion = Keys.commitAndProgress
    Reusable.implicitly((action, verb)).withValue(
      new ExtraControls(
        criterion.handle(action).toKeyHandlers,
        Instructions.Clause.keyToAction(criterion.desc)(verb, action) :: Nil))
  }

  def commitAndProgressWhenDefined(actionOption: Option[Reusable[Callback]], verb: String): Reusable[ExtraControls] =
    Reusable.implicitly((actionOption, verb)).withValue(
      _option(Keys.commitAndProgress, verb, actionOption.map(_.value)))
}
