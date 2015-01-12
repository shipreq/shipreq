package shipreq.webapp.client.app.ui.cfg.fields

import scalaz.std.option._
import shipreq.webapp.base.data._
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.app.ui.SelectOne
import SelectOne.{Choice, Props}
import TagTree.FlatRow.FilterPolicy

private[fields] class TagSelector(tt: TagTree) {

  private[this] val options = {
    val flat = TagTree.flatten(tt)(Tag.filterAlive, FilterPolicy.OmitAnythingWithBadParent)
    flat.map(f => Choice(f.id, f.indentedName, false))
  }

  private[this] val optionsO =
    SelectOne.optional(options)

  val component  = SelectOne.component[Tag.Id]
  val componentO = SelectOne.component[Option[Tag.Id]]

  def editor: SimpleEditor[Option[Tag.Id]] =
    Editor { ei =>
      val selected = ei.data
      val onSelect = ei.editable map SimpleEditor.onChangeAndEditFinished

      // Once a tag is selected, the blank option (None: Option[Tag.Id]) is removed

      selected match {
        case Some(s) => component (Props(s,    options,  onSelect.map(f => s => f(Some(s)))))
        case None    => componentO(Props(None, optionsO, onSelect))
      }
    }
}
