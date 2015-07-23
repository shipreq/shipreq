package shipreq.webapp.base.protocol

import shipreq.webapp.base.data._

object CustomIssueTypeProtocol {
  type Values = (HashRefKey, Option[String])
}

object CustomReqTypeProtocol {
  type Values = (ReqType.Mnemonic, String, ImplicationRequired)
}
