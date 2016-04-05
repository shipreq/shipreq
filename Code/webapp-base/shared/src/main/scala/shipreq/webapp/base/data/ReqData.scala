package shipreq.webapp.base.data

import nyaya.util.Multimap
import monocle.Lens
import shipreq.base.util.Optics
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._

/**
 * Data attributed to requirements beyond their basic definitions.
 */
object ReqData {

  type Text = Map[CustomField.Text.Id, Map[ReqId, Text.CustomTextField.NonEmptyText]]

  val textOuterIso =
    Optics.nonEmptyMapIso[ReqId, Text.CustomTextField.NonEmptyText]

  def textOuter(id: CustomField.Text.Id) =
    Optics.nonEmptyMapValueLens(id, textOuterIso)

  def textInner(id: ReqId) =
    Optics.nonEmptyMapValueLens(id, Text.CustomTextField.NonEmptyIso)

  def textAt(o: CustomField.Text.Id, i: ReqId): Lens[Text, Text.CustomTextField.OptionalText] =
    textOuter(o) ^|-> textInner(i)

  def emptyText: Text = Map.empty

  implicit def equalityText: UnivEq[Text] = UnivEq.univEqMap

  // -------------------------------------------------------------------------------------------------------------------

  type Tags = Multimap[ReqId, Set, ApplicableTagId]

  def emptyTags: Tags = Multimap.empty

  implicit def equalityTags: UnivEq[Tags] = univEqMultimap
}
