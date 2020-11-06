package shipreq.webapp.member.project.data

import monocle.Lens
import monocle.macros.Lenses
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.TaggedInt
/**
 * A record of the largest values used (although not necessarily in-use) as IDs. A high-water mark.
 *
 * For each value herein, adding one should always produce a never-before-used ID.
 */
@Lenses
final case class IdCeilings(customIssueType : Int,
                            customReqType   : Int,
                            customField     : Int,
                            tag             : Int,
                            req             : Int,
                            useCaseStep     : Int,
                            reqCode         : Int,
                            savedView       : Int,
    ) {

  def update(lens: Lens[IdCeilings, Int], n: Int): IdCeilings = {
    val i = lens.get(this)
    if (n > i)
      lens.set(n)(this)
    else
      this
  }
}

object IdCeilings {
  implicit def equality: UnivEq[IdCeilings] = UnivEq.derive

  def init(z: Int): IdCeilings =
    IdCeilings(z, z, z, z, z, z, z, z)

  def zero = init(0)

  def maxOf(ts: IterableOnce[TaggedInt]): Int =
    maxOfF(ts)(_.value)

  def maxOfF[F](ts: IterableOnce[F])(f: F => Int): Int =
    ts.iterator.foldLeft(0)(_ max f(_))

  /**
   * This should only be used for two reasons:
   *
   * 1) Equipping hand-written projects in unit tests or other non-production code.
   * 2) Verifying that a new calculation never results in a higher number.
   */
  def calculate(p: Project): IdCeilings = {
    def imapKeys[K <: TaggedInt](m: IMap[K, _]) = maxOf(m.keysIterator)
    IdCeilings(
      customIssueType = imapKeys(p.config.customIssueTypes),
      customReqType   = imapKeys(p.config.reqTypes.custom),
      customField     = imapKeys(p.config.fields.customFields),
      tag             = imapKeys(p.config.tags.tree),
      req             = imapKeys(p.content.reqs.genericReqs.imap) max imapKeys(p.content.reqs.useCases.imap),
      useCaseStep     = maxOfF(p.content.reqs.useCases.stepIterator)(_.id.value),
      reqCode         = maxOf(p.content.reqCodes.idSeq),
      savedView       = maxOf(p.savedViewIterator.map(_.id)),
    )
  }

  /**
   * Similar to `calculate()`, this should not be used in production code.
   */
  def supply(f: IdCeilings => Project): Project = {
    val p0 = f(zero)
    p0.copy(idCeilings = calculate(p0))
  }
}