package shipreq.webapp.base

import shipreq.webapp.base.lib.BrowserStorage

object GlobalSettings {

  private implicit def storage = BrowserStorage.localOrEmpty

  val SessionExpired = BrowserStorage.Field.boolean("session-expired")
}
