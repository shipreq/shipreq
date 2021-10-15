package shipreq.webapp.member.project.data

import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.macros.Lenses
import monocle.{Iso, Lens}
import shipreq.base.util.Optics
import shipreq.webapp.member.project.text.Text.Equality._
import shipreq.webapp.member.project.text.{Text => T}

/**
 * Data attributed to requirements beyond their basic definitions.
 */
object ReqData {

  @Lenses
  final case class Text(data: Map[CustomField.Text.Id, Map[ReqId, T.CustomTextField.NonEmptyText]]) {

    private[data] lazy val localCodeRefs =
      derivation.AtomScan.reqCodeRefs { f =>
        for {
          (_, textByReqId) <- data
          (_, txt)         <- textByReqId
        } f(txt.whole)
      }

    private[data] lazy val localUseCaseStepRefs =
      derivation.AtomScan.useCaseStepRefs { f =>
        for {
          (_, textByReqId) <- data
          (_, txt)         <- textByReqId
        } f(txt.whole)
      }

    // Only used in tests
    def allTextForReq(id: ReqId): Map[CustomField.Text.Id, T.CustomTextField.NonEmptyText] =
      data.iterator
        .filter(_._2.contains(id))
        .map(_.map2(_ apply id))
        .toMap

  }

  object Text {

    val empty: Text =
      apply(Map.empty)

    implicit def univEq: UnivEq[Text] =
      UnivEq.derive

    private val outerIso: Iso[
      Option[Map[ReqId, T.CustomTextField.NonEmptyText]],
      Map[ReqId, T.CustomTextField.NonEmptyText]] =
      Optics.nonEmptyMapIso[ReqId, T.CustomTextField.NonEmptyText]

    private def outer(id: CustomField.Text.Id): Lens[Text, Map[ReqId, T.CustomTextField.NonEmptyText]] =
      data andThen Optics.nonEmptyMapValueLens(id, outerIso)

    private def inner(id: ReqId): Lens[Map[ReqId, T.CustomTextField.NonEmptyText], T.CustomTextField.OptionalText] =
      Optics.nonEmptyMapValueLens(id, T.CustomTextField.NonEmptyIso)

    def at(o: CustomField.Text.Id, i: ReqId): Lens[Text, T.CustomTextField.OptionalText] =
      outer(o) andThen inner(i)
  }

  // -------------------------------------------------------------------------------------------------------------------

  type Tags = Multimap[ReqId, Set, ApplicableTagId]

  def emptyTags: Tags =
    Multimap.empty

  implicit def equalityTags: UnivEq[Tags] =
    univEqMultimap
}
