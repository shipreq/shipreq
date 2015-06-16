package shipreq.webapp.client.lib

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
sealed trait Contextualise

case object Contextualise extends Contextualise with IsoBool.Obj[Contextualise] {
  override protected def neg = Plain
}

case object Plain extends Contextualise with IsoBool[Contextualise] {
  override protected def neg = Contextualise
}