package shipreq.webapp.client.app.ui

import scalaz.Equal
import scalaz.std.option._
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.util.Enabled
import SelectOne.{Props, Choice}

/**
 * Same as [[SelectOne]] except that it can be initialised to empty (no choice).
 * Once a choice is selected however, the user can not change it back to empty.
 */
class SelectOneStartNone[A: Equal] private[SelectOneStartNone](options: Vector[Choice[A]]) {

  private[this] val optionsN =
    NonEmptyVector.option(options)

  private[this] val optionsO =
    SelectOne.optional(options)

  val ComponentA = SelectOne.Component[A]
  val ComponentO = SelectOne.Component[Option[A]]

  def editor: SimpleEditor[Option[A]] =
    Editor { ei =>
      val selected = ei.data
      val onSelect = ei.editable map SimpleEditor.onChangeAndEditFinished

      // Once a tag is selected, the blank option (None: Option[TagId]) is removed

      (selected, optionsN) match {
        case (Some(s), Some(o)) => ComponentA(Props(s,    o,        onSelect.map(f => s => f(Some(s)))))
        case _                  => ComponentO(Props(None, optionsO, onSelect))
      }
    }
}

// TODO SelectOneStartNone creates new components when input changes which messes with React and browser focus.
object SelectOneStartNone {
  import shipreq.webapp.base.data._

  def reqType(rs: TraversableOnce[ReqType]): SelectOneStartNone[ReqTypeId] = {
    val opts = rs.toStream
                .map(r => Choice(r.reqTypeId, r.name, Enabled))
                .sortBy(_.label)
                .toVector
    new SelectOneStartNone(opts)
  }

  def tag(tt: TagTree): SelectOneStartNone[TagId] = {
    import FlatTag._
    val flat = flatten(tt)(Tag.filterLive, FilterPolicy.OmitAnythingWithBadParent)
    val opts = flat.map(f => Choice(f.id, f.indentedName, Enabled))
    new SelectOneStartNone(opts)
  }
}