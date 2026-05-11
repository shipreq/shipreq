package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react._
import shipreq.webapp.base.config.Urls
import shipreq.webapp.base.lib.AbstractLocation
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.ww.api.WebWorkerCmd

/** @param onRevoke Procedure to execute when the current user's access is revoked. */
final case class AccessHandler(onRevoke: Callback)

object AccessHandler {

  def default(ww : WebWorkerClient.Instance,
              loc: AbstractLocation): AccessHandler = {

    val onRevoke: AsyncCallback[Unit] =
      for {
        _ <- ww.send(WebWorkerCmd.ClearAndDisableCache).timeoutMs(12000).attempt
        _ <- loc.setHrefRelative(Urls.projectAccessRevoked).asAsyncCallback
      } yield ()

    AccessHandler(onRevoke.toCallback)
  }
}