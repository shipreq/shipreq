package shipreq.webapp.server.logic

import shipreq.webapp.base.data.ProjectId

object Obfuscators {

  val projectId: Obfuscator[ProjectId] =
    Obfuscator.long("F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")
      .xmap(ProjectId.apply)(_.value)

}
