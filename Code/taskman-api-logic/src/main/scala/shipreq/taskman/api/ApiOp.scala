package shipreq.taskman.api

/**
 * Represents an operation provided by the API.
 *
 * @tparam A The operation result.
 */
sealed trait ApiOp[A]

object ApiOp {

  /**
   * Submits a Msg to the Taskman server for processing.
   */
  case class SubmitMsg(m: Msg) extends ApiOp[MsgId]

  /**
   * Submits 0-n Msgs to the Taskman server for processing.
   */
  case class SubmitMsgs(ms: Seq[Msg]) extends ApiOp[Unit]

  /**
   * Stores/updates a configuration value that will be read by the Taskman server.
   *
   * @param k The config key.
   * @param v The config value.
   */
  case class CfgPut(k: String, v: String) extends ApiOp[Unit]
}
