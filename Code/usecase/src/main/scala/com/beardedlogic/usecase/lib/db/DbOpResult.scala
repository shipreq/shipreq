package com.beardedlogic.usecase.lib.db

import DbOpResult._

/**
 * Indicates a type of result of a high-level database operation.
 *
 * @since 14/06/2013
 */
sealed trait DbOpResult[+T] {
  def isSuccess: Boolean
  @inline final def isFailure = !isSuccess
  def toSuccess: Option[Success[T]]
  def toFailure: Option[Failure]
  def dataOpt: Option[T]
  def successCodeOpt = toSuccess.map(_.code)
}

object DbOpResult {

  /** Database operation was a success. */
  case class Success[+T](code: SuccessCode, data: T) extends DbOpResult[T] {
    override def isSuccess = true
    override def dataOpt = Some(data)
    override def toSuccess = Some(this)
    override def toFailure = None
  }

  sealed trait SuccessCode

  /** Database operation failed. */
  sealed trait Failure extends DbOpResult[Nothing] {
    override def isSuccess = false
    override def dataOpt = None
    override def toSuccess = None
    override def toFailure = Some(this)
  }

  /** A new revision of a value was successfully created. */
  case object NewRevision extends SuccessCode

  /** A value was successfully updated directly. No new revision or audit trail was created. */
  case object DirectUpdate extends SuccessCode

  /** The database operation was determined to have no effect on the data. */
  case object AlreadyUpToDate extends SuccessCode

  /** The data used in the request is now out-of-date, therefore the operation was aborted. */
  case object StaleRevision extends Failure

  /** A database CONSTRAINT was violated. */
  case object ConstraintViolation extends Failure

  /** An UPDATE statement didn't affect anything, ie. nothing matched the UPDATE's WHERE clause. */
  case object NothingUpdated extends Failure

}
