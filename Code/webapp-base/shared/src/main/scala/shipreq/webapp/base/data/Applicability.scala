package shipreq.webapp.base.data

import japgolly.univeq.UnivEq
import shipreq.base.util.{Applicable, Memo}

/**
  * @see [[ProjectConfig.applicability]] for a starting point
  */
final case class Applicability[-Field, -Data](byField: Field => Data => Applicable) {
  // Doesn't extend AnyVal because it causes boxing/unboxing in Reusability

  /** Not all fields are applicable to all types of requirements.
    *
    * For example, there could be a field called "Business contact" that might apply to BRs (business requirements)
    * but no SIs (solution ideas).
    */
  def apply(data: Data, field: Field): Applicable =
    byField(field)(data)

  def contramapData[A](f: A => Data): Applicability[Field, A] =
    Applicability(byField(_) compose f)

  def mapDataFn[F <: Field, A](f: (F, Data => Applicable) => A => Applicable): Applicability[F, A] =
    Applicability(field => f(field, byField(field)))

  def memoiseByField[F <: Field](implicit u: UnivEq[F]): Applicability[F, Data] =
    Applicability(Memo[F, Data => Applicable](byField))
}

object Applicability {
  type Default = Applicability[FieldId, ReqTypeId]
}