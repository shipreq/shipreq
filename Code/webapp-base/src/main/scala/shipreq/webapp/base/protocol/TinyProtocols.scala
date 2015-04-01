package shipreq.webapp.base.protocol

import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.{Partition, GenericPartitionFns}

object CustomIssueTypeProtocol {
  type Values = (HashRefKey, Option[String])
  val partitionFns = GenericPartitionFns(Partition.CustomIssueTypes, Project.customIssueTypes)
}

object CustomReqTypeProtocol {
  type Values = (ReqType.Mnemonic, String, ImplicationRequired)
  val partitionFns = GenericPartitionFns(Partition.CustomReqTypes, Project.customReqTypes)
}
