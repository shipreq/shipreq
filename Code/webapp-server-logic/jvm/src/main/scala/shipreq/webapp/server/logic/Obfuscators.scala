package shipreq.webapp.server.logic

import shipreq.webapp.base.data.{ProjectId, UserId}

object Obfuscators {

  val projectId: Obfuscator[ProjectId] =
    Obfuscator.long("F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")
      .xmap(ProjectId.apply)(_.value)

  val userId: Obfuscator[UserId] =
    Obfuscator.long("RmlNfjvVuwxkePSYq3Acdib6yor0zIg948MOFTLG2B7tHnsKap5XCJ1DEZhUQW")
      .xmap(UserId.apply)(_.value)

}
