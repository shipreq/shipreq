package shipreq.webapp.base.data

import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.util.Multimap
import monocle.{Iso, Lens}
import shipreq.base.util.Optics
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._

/**
 * Data attributed to requirements beyond their basic definitions.
 */
object ReqData {

  type Text = Map[CustomField.Text.Id, Map[ReqId, Text.CustomTextField.NonEmptyText]]

  // Only used in tests
  def allTextForReq(id: ReqId, text: Text): Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText] =
    text.iterator
      .filter(_._2.contains(id))
      .map(_.map2(_ apply id))
      .toMap

  val textOuterIso: Iso[
      Option[Map[ReqId, Text.CustomTextField.NonEmptyText]],
             Map[ReqId, Text.CustomTextField.NonEmptyText]] =
    Optics.nonEmptyMapIso[ReqId, Text.CustomTextField.NonEmptyText]

  def textOuter(id: CustomField.Text.Id): Lens[Text, Map[ReqId, Text.CustomTextField.NonEmptyText]] =
    Optics.nonEmptyMapValueLens(id, textOuterIso)

  def textInner(id: ReqId): Lens[Map[ReqId, Text.CustomTextField.NonEmptyText], Text.CustomTextField.OptionalText] =
    Optics.nonEmptyMapValueLens(id, Text.CustomTextField.NonEmptyIso)

  def textAt(o: CustomField.Text.Id, i: ReqId): Lens[Text, Text.CustomTextField.OptionalText] =
    textOuter(o) ^|-> textInner(i)

  def emptyText: Text =
    Map.empty

  implicit def equalityText: UnivEq[Text] =
    UnivEq.univEqMap

  // -------------------------------------------------------------------------------------------------------------------

  type Tags = Multimap[ReqId, Set, ApplicableTagId]

  def emptyTags: Tags =
    Multimap.empty

  implicit def equalityTags: UnivEq[Tags] =
    univEqMultimap
}
