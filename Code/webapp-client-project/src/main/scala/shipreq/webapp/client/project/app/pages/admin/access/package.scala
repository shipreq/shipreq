package shipreq.webapp.client.project.app.pages.admin

import shipreq.webapp.base.data.UserId

package object access {

  type AsyncKey = Option[UserId.Public]

  object AsyncKey {
    @inline def newUser: AsyncKey =
      None

    @inline def apply(id: UserId.Public): AsyncKey =
      Some(id)
  }

}
