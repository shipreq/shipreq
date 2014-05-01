package shipreq.taskman.server.app

import shipreq.taskman.api.impl.Serialisation

/**
 * Submits message(s) specified on the command line.
 */
object ManualSubmit extends ManualSubmitBase with MainTemplate {
  override def serialise   = Serialisation.serialise _
  override def deserialise = Serialisation.deserialise _
  override def runner      = f => withTaskmanCtx(ctx => f(ctx.aopReifier))
}