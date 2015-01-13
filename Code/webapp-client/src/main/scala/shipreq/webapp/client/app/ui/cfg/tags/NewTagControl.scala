package shipreq.webapp.client.app.ui.cfg.tags

import scalaz.effect.IO
import shipreq.webapp.base.data.TagType
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.{SelectInvoke, SelectOne}
import SelectOne.Choice

private[tags] object NewTagControl {

  val choices = TagType.values.map(tt => Choice(tt, tt.name, false))

  val Component = SelectInvoke.Component[TagType]("NewTag")

  def props(selected: TagType,
            invoke  : Option[IO[Unit]],
            select  : TagType => IO[Unit],
            disabled: Boolean): SelectInvoke.Props[TagType] =

    SelectInvoke.Props(
      SelectOne.Props(selected, choices, Some(select)),
      invoke, UiText.Cfg.startNewButton, disabled)
}
