package shipreq.webapp.base.data

import japgolly.microlibs.utils.Memo
import japgolly.univeq.UnivEq
import shipreq.base.util.Applicability

/**
  * @see [[ProjectConfig.applicability]] for a starting point
  */
final case class ProjectApplicability[-Field, -Data](byField: Field => Data => Applicability) {
  // Doesn't extend AnyVal because it causes boxing/unboxing in Reusability

  /** Not all fields are applicable to all types of requirements.
    *
    * For example, there could be a field called "Business contact" that might apply to BRs (business requirements)
    * but no SIs (solution ideas).
    */
  def apply(data: Data, field: Field): Applicability =
    byField(field)(data)

  def contramapData[A](f: A => Data): ProjectApplicability[Field, A] =
    ProjectApplicability(byField(_) compose f)

  def mapDataFn[F <: Field, A](f: (F, Data => Applicability) => A => Applicability): ProjectApplicability[F, A] =
    ProjectApplicability(field => f(field, byField(field)))

  def memoiseByField[F <: Field](implicit u: UnivEq[F]): ProjectApplicability[F, Data] =
    ProjectApplicability(Memo[F, Data => Applicability](byField))
}

object ProjectApplicability {
  type Default = ProjectApplicability[FieldId, ReqTypeId]
}