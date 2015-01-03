package shipreq.webapp.base

trait UnsafeTypesLowPriority {
  // implicit def autoSome[A, B](a: A)(implicit f: A => B): Option[B] = Some(f(a))
  implicit def autoSome[A](a: A): Option[A] = Some(a)
}

/**
 * THIS SHOULD ONLY BE USED FOR TESTING.
 */
object UnsafeTypes extends UnsafeTypesLowPriority {
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.delta._

  implicit def autoMnemonic(s: String) = ReqType.Mnemonic(s)
  implicit def autoRefKey(s: String) = RefKey(s)

  implicit def autoCustomReqTypeId  (i: Int) = CustomReqType.Id(i)
  implicit def autoCustomIssueTypeId(i: Int) = CustomIssueType.Id(i)
  implicit def autoTagId            (i: Int) = Tag.Id(i)
  implicit def autoRev              (i: Int) = Rev(i)

  implicit def autoCustomReqTypeIdO  (i: Int): Option[CustomReqType.Id]   = Some(i)
  implicit def autoCustomIssueTypeIdO(i: Int): Option[CustomIssueType.Id] = Some(i)
  implicit def autoTagIdO            (i: Int): Option[Tag.Id]             = Some(i)

  implicit def tagTreeTree(t: TagTree) = t.mapValues(_.children)
}
