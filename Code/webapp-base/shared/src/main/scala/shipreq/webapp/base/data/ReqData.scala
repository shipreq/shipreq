package shipreq.webapp.base.data

import japgolly.nyaya.util.Multimap
import monocle.Lens
import shipreq.base.util.UnivEq
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.util.Optics

/**
 * Data attributed to requirements beyond their basic definitions.
 */
object ReqData {

  type Text = Map[CustomField.Text.Id, Map[ReqId, Text.CustomTextField.NonEmptyText]]

  val textOuterIso =
    Optics.nonEmptyMapIso[ReqId, Text.CustomTextField.NonEmptyText]

  def textOuter(id: CustomField.Text.Id) =
    Optics.nonEmptyMapValue(id, textOuterIso)

  def textInner(id: ReqId) =
    Optics.nonEmptyMapValue(id, Text.CustomTextField.NonEmptyIso)

  def textAt(o: CustomField.Text.Id, i: ReqId): Lens[Text, Text.CustomTextField.OptionalText] =
    textOuter(o) ^|-> textInner(i)

  def emptyText: Text = Map.empty

  implicit def equalityText: UnivEq[Text] = UnivEq.univEqMap

  // -------------------------------------------------------------------------------------------------------------------

  type Tags = Multimap[ReqId, Set, ApplicableTagId]

  def emptyTags: Tags = Multimap.empty

  implicit def equalityTags: UnivEq[Tags] = UnivEq.univEqMultimap
}
