package shipreq.webapp.member.project.data

import shipreq.base.util.IsoBool

/**
 * Does a subject have/need context?
 *
 * Used to denote whether a datum can be displayed/parsed as a plain value, or whether it needs some context.
 *
 * Examples:
 *
 * - Requirement pubids - `MF-17` vs `[MF-17]`.
 * - Tags - `defer` vs `#defer`.
 */
sealed trait Contextualise extends IsoBool[Contextualise] {
  override final def companion = Contextualise
}

case object Contextualise extends Contextualise with IsoBool.Object[Contextualise] {
  override def positive = Contextualise
  override def negative = Plain
}

case object Plain extends Contextualise
