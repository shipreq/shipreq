package shipreq.webapp.base.data

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq.UnivEq
import monocle.Traversal
import scalaz.Applicative
import shipreq.base.util.{Applicability, Applicable, Impossible, NotApplicable => NA}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.FieldReqTypeRules._

/** Rules for fields settings per req type.
 *
 * Eg.
 *
 *     [ FR MF ] -- Mandatory
 *     [ CO SI ] -- Not applicable
 *     [ XX    ] -- Default to #new
 *     Otherwise -- Optional
 *
 * @since v2.1 */
final case class FieldReqTypeRules[+D](perReqType: Map[ReqTypeId, Resolution[D]], otherwise: Resolution[D]) {

  def apply(id: ReqTypeId): Resolution[D] =
    perReqType.getOrElse(id, otherwise)

  def -(id: ReqTypeId): FieldReqTypeRules[D] =
    FieldReqTypeRules(perReqType - id, otherwise)

  def resolutionIterator(): Iterator[Resolution[D]] =
    perReqType.valuesIterator ++ Iterator.single(otherwise)

  def resolutionIterator(reqTypeFilter: ReqTypeId => Boolean): Iterator[Resolution[D]] =
    Iterator.single(otherwise) ++ perReqType.iterator.filter(x => reqTypeFilter(x._1)).map(_._2)

  def liveResolutionIterator(reqTypes: ReqTypes): Iterator[Resolution[D]] =
    resolutionIterator(reqTypes.need(_).live is Live)

  def liveIterator(reqTypes: ReqTypes): Iterator[(ReqType, Resolution[D])] =
    reqTypes.all
      .iterator
      .filter(_.live is Live)
      .map(r => (r, apply(r.reqTypeId)))

  def foreach(f: (Option[ReqTypeId], Resolution[D]) => Unit): Unit = {
    for ((rt, res) <- perReqType)
      f(Some(rt), res)
    f(None, otherwise)
  }

  val containsMandatory: Boolean =
    resolutionIterator().contains(Resolution.Mandatory)

  def updated[DD >: D](ids: ReqTypeId*)(res: Resolution[DD]): FieldReqTypeRules[DD] =
    copy(ids.foldLeft(perReqType: Map[ReqTypeId, Resolution[DD]])(_.updated(_, res)))

  def defaultTo[DD >: D](d: DD)(ids: ReqTypeId*): FieldReqTypeRules[DD] =
    updated[DD](ids: _*)(Resolution.DefaultTo(d))

  def optional(ids: ReqTypeId*): FieldReqTypeRules[D] =
    updated(ids: _*)(Resolution.Optional)

  def mandatory(ids: ReqTypeId*): FieldReqTypeRules[D] =
    updated(ids: _*)(Resolution.Mandatory)

  def notApplicable(ids: ReqTypeId*): FieldReqTypeRules[D] =
    updated(ids: _*)(Resolution.NotApplicable)

  def hardDelete(id: ReqTypeId): FieldReqTypeRules[D] =
    if (perReqType contains id)
      FieldReqTypeRules(perReqType - id, otherwise)
    else
      this

  private[data] def byResolution[DD >: D]: FieldReqTypeRules.ByResolution[DD] = {
    var perRes = Map.empty[Resolution[DD], NonEmptySet[ReqTypeId]]
    for ((id, res) <- perReqType) {
      val newIds = perRes.get(res) match {
        case Some(ids) => ids + id
        case None      => NonEmptySet one id
      }
      perRes = perRes.updated(res, newIds)
    }
    ByResolution(perRes, otherwise)
  }
}

object FieldReqTypeRules {

  val empty: FieldReqTypeRules[Nothing] =
    apply(Map.empty, Resolution.default)

  def resolutionTraversal[D]: Traversal[FieldReqTypeRules[D], Resolution[D]] =
    new Traversal[FieldReqTypeRules[D], Resolution[D]] {
      override def modifyF[F[_]](f: Resolution[D] => F[Resolution[D]])(s: FieldReqTypeRules[D])(implicit F: Applicative[F]): F[FieldReqTypeRules[D]] = {
        val fMap: F[Map[ReqTypeId, Resolution[D]]] =
          s.perReqType
            .iterator
            .map { case (k, v) => F.map(f(v))((k, _)) }
            .foldLeft(F.pure(Map.empty[ReqTypeId, Resolution[D]]))(F.apply2(_, _)(_ + _))

        val fOtherwise =
          f(s.otherwise)

        F.apply2(fMap, fOtherwise)(FieldReqTypeRules(_, _))
      }
    }

  def const[D](res: Resolution[D]): FieldReqTypeRules[D] =
    FieldReqTypeRules(Map.empty, res)

  def defaultTo[D](d: D) = const(Resolution.DefaultTo(d))
  def optional           = const(Resolution.Optional)
  def mandatory          = const(Resolution.Mandatory)
  def notApplicable      = const(Resolution.NotApplicable)

  def only[D](reqTypeId: ReqTypeId, resolution: Resolution[D]): FieldReqTypeRules[D] =
    FieldReqTypeRules(Map(reqTypeId -> resolution), Resolution.NotApplicable)

  type ForImpField  = FieldReqTypeRules[Impossible]
  type ForTagField  = FieldReqTypeRules[ApplicableTagId]
  type ForTextField = FieldReqTypeRules[Impossible]

  sealed abstract class Resolution[+Default](final val applicability: Applicability) {
    final def isNA = applicability is NA
  }
  
  object Resolution {
    case object Mandatory                      extends Resolution[Nothing](Applicable)
    case object Optional                       extends Resolution[Nothing](Applicable)
    case object NotApplicable                  extends Resolution[Nothing](NA)
    final case class DefaultTo[+D](default: D) extends Resolution[D]      (Applicable)

    @inline def default = Optional

    type ForImpField  = Resolution[Impossible]
    type ForTagField  = Resolution[ApplicableTagId]
    type ForTextField = Resolution[Impossible]

    def v1(mandatory: data.Mandatory) =
      mandatory match {
        case data.Mandatory     => Mandatory
        case data.Mandatory.Not => Optional
      }
  }

  def v1(mandatory: Mandatory, art: ApplicableReqTypes): FieldReqTypeRules[Nothing] = {

    val on =
      if (mandatory is Mandatory)
        Resolution.Mandatory
      else
        Resolution.Optional

    if (art.isEmpty)
      apply(Map.empty, on)
    else
      art.applicability match {
        case Applicable =>
          // Whitelist X, Y, Z
          apply(art.reqTypes.iterator.map(_ -> on).toMap, Resolution.NotApplicable)

        case NA =>
          // Blacklist X, Y, Z
          apply(art.reqTypes.iterator.map(_ -> Resolution.NotApplicable).toMap, on)
      }
  }

  final case class ByResolution[D](perRes: Map[Resolution[D], NonEmptySet[ReqTypeId]], otherwise: Resolution[D]) {
    lazy val toRules: FieldReqTypeRules[D] = {
      val byId =
        perRes.iterator.flatMap { case (res, ids) =>
          ids.iterator.map((_, res))
        }.toMap
      FieldReqTypeRules(byId, otherwise)
    }
  }

  implicit def univEqResolution        [D: UnivEq]: UnivEq[Resolution       [D]] = UnivEq.derive
  implicit def univEqByResolution      [D: UnivEq]: UnivEq[ByResolution     [D]] = UnivEq.derive
  implicit def univEqFieldReqTypeRules [D: UnivEq]: UnivEq[FieldReqTypeRules[D]] = UnivEq.derive
}
