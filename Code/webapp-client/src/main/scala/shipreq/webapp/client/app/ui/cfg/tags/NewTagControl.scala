package shipreq.webapp.client.app.ui.cfg.tags

import scalaz.effect.IO
import shipreq.webapp.base.data.TagType
import shipreq.webapp.client.app.ui.{SelectAndInvoke, SelectOne}
import SelectOne.Choice

private[tags] object NewTagControl {

  val choices = TagType.values.map(tt => Choice(tt, tt.name, false))

  val Component = SelectAndInvoke.Component[TagType]("NewTag")

  def props(selected: TagType,
            invoke  : Option[IO[Unit]],
            select  : TagType => IO[Unit],
            disabled: Boolean): SelectAndInvoke.Props[TagType] =

    SelectAndInvoke.Props(
      SelectOne.Props(selected, choices, Some(select)),
      "Create", invoke, disabled) // TODO sync all new buttons
}
