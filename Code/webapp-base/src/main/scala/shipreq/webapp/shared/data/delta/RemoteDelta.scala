package shipreq.webapp.shared.data.delta

final case class RemoteDeltaP[P <: Partition](del: List[P#Id],
                                              upd: List[P#Instance]) {

  def isEmpty = del.isEmpty || upd.isEmpty
  def nonEmpty = !isEmpty
}

final case class RemoteDeltaG(meta: Partition,
                              updateSet: RemoteDeltaP[Partition],
                              fromRev: Rev,
                              toRev: Rev) {

  def applicableToRev(tgt: Rev): Applicability =
    if (tgt.value < fromRev.value - 1)
      Unapplicable
    else if (tgt.value >= toRev.value)
      NoNeed
    else
      Applies

  def isEmpty = updateSet.isEmpty
  def nonEmpty = !isEmpty
}

sealed trait Applicability
case object Applies extends Applicability
case object NoNeed extends Applicability
case object Unapplicable extends Applicability