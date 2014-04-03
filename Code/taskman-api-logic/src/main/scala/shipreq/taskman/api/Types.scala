package shipreq.taskman.api

import shipreq.base.util.TypeTags

// TODO Merge with webapp's types
// TODO use value classes

case class MsgId(value: Long) extends AnyVal

trait Types extends TypeTags {
  import Types._
  type UserId = JLong @@ IsUserId
  type EmailAddr = String @@ IsEmailAddr
}

object Types extends Types {
  sealed trait IsUserId extends TypeTag[JLong]
  sealed trait IsEmailAddr extends TypeTag[String]
}
