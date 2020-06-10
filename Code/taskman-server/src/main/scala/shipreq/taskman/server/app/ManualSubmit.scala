package shipreq.taskman.server.app

import shipreq.taskman.server.logic.app.ManualSubmitLogic

/**
 * Submits message(s) specified on the command line.
 */
object ManualSubmit extends ManualSubmitLogic with TaskmanApp {

  override protected def runner =
    f => withTaskmanCtx(ctx => f(ctx.taskmanApi))
}