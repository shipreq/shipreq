package shipreq.taskman.server.app

import shipreq.taskman.server.logic.app.ManualSubmitBase

/**
 * Submits message(s) specified on the command line.
 */
object ManualSubmit extends ManualSubmitBase with MainTemplate {
  override def runner = f => withTaskmanCtx(ctx => f(ctx.taskmanApi))
}