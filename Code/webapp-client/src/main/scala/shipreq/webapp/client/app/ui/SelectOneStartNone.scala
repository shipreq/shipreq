package shipreq.webapp.client.app.ui

import scalaz.Equal
import scalaz.std.option._
import shipreq.webapp.client.lib.ui._
import SelectOne.{Props, Choice}

/**
 * Same as [[SelectOne]] except that it can be initialised to empty (no choice).
 * Once a choice is selected however, the user can not change it back to empty.
 */
class SelectOneStartNone[A: Equal](options: Vector[Choice[A]]) {

  private[this] val optionsO =
    SelectOne.optional(options)

  val ComponentA = SelectOne.Component[A]
  val ComponentO = SelectOne.Component[Option[A]]

  def editor: SimpleEditor[Option[A]] =
    Editor { ei =>
      val selected = ei.data
      val onSelect = ei.editable map SimpleEditor.onChangeAndEditFinished

      // Once a tag is selected, the blank option (None: Option[Tag.Id]) is removed

      selected match {
        case Some(s) => ComponentA(Props(s,    options,  onSelect.map(f => s => f(Some(s)))))
        case None    => ComponentO(Props(None, optionsO, onSelect))
      }
    }
}

object SelectOneStartNone {
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.data.TagTree.FlatRow.FilterPolicy

  def reqType(rs: TraversableOnce[ReqType]): SelectOneStartNone[ReqTypeId] = {
    val opts = rs.toStream
                .map(r => Choice(r.reqTypeId, r.name, disabled = false))
                .sortBy(_.label)
                .toVector
    new SelectOneStartNone(opts)
  }

  def tag(tt: TagTree): SelectOneStartNone[Tag.Id] = {
    val flat = TagTree.flatten(tt)(Tag.filterAlive, FilterPolicy.OmitAnythingWithBadParent)
    val opts = flat.map(f => Choice(f.id, f.indentedName, disabled = false))
    new SelectOneStartNone(opts)
  }
}