package shipreq.webapp.client.app.ui.cfg.fields

import scalaz.std.option._
import shipreq.webapp.base.data._
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.app.ui.SelectOne
import SelectOne.{Choice, Props}
import TagTree.FlatRow.FilterPolicy

private[fields] class TagSelector(tt: TagTree) {

  private[this] val options = {
    val flat    = TagTree.flatten(tt)(Tag.filterAlive, FilterPolicy.OmitAnythingWithBadParent)
    val choices = flat.map(f => Choice(f.id, f.tag.name, false))
    SelectOne.optional(choices)
  }

  val component = SelectOne.component[Option[Tag.Id]]

  def editor: SimpleEditor[Option[Tag.Id]] =
    Editor { ei =>
      val selected = ei.data
      val onSelect = ei.editable map SimpleEditor.onChangeAndEditFinished
      component(Props(selected, options, onSelect))
    }
}
