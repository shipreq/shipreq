package shipreq.webapp.base.feature.editcontrols

import shipreq.webapp.base.lib.KeyHandler.Criterion._

object Keys {

  @inline def abort = Escape

  /** It used to be the case that in single-line editors, Enter would be used to commit with Ctrl-Enter also allowed
   * for consistency with multi-line editors.
   * It's much simpler for a user to just remember that Ctrl-Enter is commit every and that Enter works like it does
   * in a text editor, just not everywhere. The penalty for trying to insert a newline into a single-line editor is
   * now nil, in that nothing happens; where as previously it would trigger a save which can be very annoying.
   */
  @inline def commit = CtrlEnter

  /** Commit and progress, as in "save and let's move on".
   *
   * Progress is different depending on the context.
   * For UC steps, it means close the current step, create a new child step and focus it.
   * For fields in the ReqTable new requirement form, it means save and move onto next new req (i.e. keep open).
   */
  @inline def commitAndProgress = AltEnter

}
