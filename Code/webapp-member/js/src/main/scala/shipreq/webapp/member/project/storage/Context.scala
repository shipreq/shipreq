package shipreq.webapp.member.project.storage

import shipreq.webapp.base.data.{ProjectId, UserId}
import shipreq.webapp.member.project.data.ClientSideProjectEncryptionKey

final case class Context(userId   : UserId.Public,
                         projectId: ProjectId.Public,
                         encKey   : ClientSideProjectEncryptionKey,
                        ) {

  val namespace: String =
    s"${userId.value}:${projectId.value}"
}
