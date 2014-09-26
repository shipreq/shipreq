package shipreq.webapp.shared.rpc

/**
 * Describes functions exposed in client JS, that the server can invoke.
 *
 * The contents of this object allow client and server to be in sync wrt exported JS names and parameter types.
 */
object ClientAccess {
  final class Fn[I,O] private[ClientAccess] (val name: String)

  final val client = "Bnzklx"

  final val reactExamplesN = "x8927nh"
  final val reactExamples = new Fn[Interfaces.WIP, Unit](reactExamplesN)

}
