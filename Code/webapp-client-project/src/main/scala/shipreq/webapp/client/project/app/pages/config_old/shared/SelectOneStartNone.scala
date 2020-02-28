package shipreq.webapp.client.project.app.pages.config_old.shared

import scalaz.Equal
import scalaz.std.option._
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.data.Enabled
import shipreq.webapp.client.project.widgets.SelectOne
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
        case (Some(s), Some(o)) => ComponentA(Props(s, o, onSelect.map(f => s => f(Some(s)))))
        case _                  => ComponentO(Props(None, optionsO, onSelect))
      }
    }
}

// TODO SelectOneStartNone creates new components when input changes which messes with React and browser focus.
object SelectOneStartNone {
  import shipreq.webapp.base.data._

  def reqType(rs: TraversableOnce[ReqType]): SelectOneStartNone[ReqTypeId] = {
    val opts = rs.toIterator
                .map(r => Choice(r.reqTypeId, r.name, Enabled))
                .toVector
                .sortBy(_.label)
    new SelectOneStartNone(opts)
  }

  def tag(tags: Tags): SelectOneStartNone[TagId] = {
    val opts = tags.flatRowsLive.map(f => Choice(f.id, f.indentedName, Enabled))
    new SelectOneStartNone(opts)
  }
}
