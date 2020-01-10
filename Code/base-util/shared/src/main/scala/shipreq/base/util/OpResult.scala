package shipreq.base.util

sealed abstract class OpResult extends IsoBool.WithBoolOps[OpResult] {
  override final def companion = OpResult
}

object OpResult extends IsoBool.Object[OpResult] {
  case object Success extends OpResult
  case object Failure extends OpResult

  override def positive = Success
  override def negative = Failure

  def isSuccess: OpResult => Boolean = _ is Success
  def isFailure: OpResult => Boolean = _ is Failure
}
