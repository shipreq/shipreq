package shipreq.webapp.server.redis

import shipreq.webapp.base.data.ProjectId

final case class RedisSchema(prefix: String) {
  def snapshot(pid: ProjectId): String = prefix + pid.value + ":ss"
  def events  (pid: ProjectId): String = prefix + pid.value + ":es"
  def topic   (pid: ProjectId): String = prefix + pid.value + ":topic"
}

object RedisSchema {
  def default = RedisSchema("project:")
}
