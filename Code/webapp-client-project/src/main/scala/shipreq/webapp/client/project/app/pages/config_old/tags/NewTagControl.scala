package shipreq.webapp.client.project.app.pages.config_old.tags

import japgolly.scalajs.react.Callback
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.TagType
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.Enabled
import shipreq.webapp.client.project.widgets.{SelectInvoke, SelectOne}
import SelectOne.Choice

private[tags] object NewTagControl {

  val choices = TagType.values.map(tt => Choice(tt, tt.name, Enabled))

  val Component = SelectInvoke.Component[TagType]("NewTag")

  def props(selected: TagType,
            invoke  : Option[Callback],
            select  : TagType => Callback,
            enabled : Enabled): SelectInvoke.Props[TagType] =

    SelectInvoke.Props(
      SelectOne.Props(selected, choices, Some(select)),
      invoke, UiText.Cfg.startNewButton, enabled)
}
