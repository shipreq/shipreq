package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.ReqTypes
import shipreq.webapp.client.project.feature.CreateFeature.RowKey
import shipreq.webapp.client.project.widgets.ButtonAndDropdown

object NewButton extends ButtonAndDropdown.Types[RowKey] {

  type State = Option[RowKey]

  @inline def initState: State =
    None

  final case class Props(state   : State,
                         reqTypes: ReqTypes,
                         allowRCG: Permission,
                         default : Option[RowKey],
                         update  : Option[Reusable[Update]]) {

    private lazy val items: NonEmptyVector[Item] = {

      var items: NonEmptyVector[Item] =
        reqTypes.liveSortedByMnemonic.map(rt =>
          Item(
            key              = rt.mnemonic.value,
            value            = RowKey.req(rt.reqTypeId),
            renderInButton   = rt.mnemonic.value,
            renderInDropdown = s"${rt.mnemonic.value}: ${rt.name}",
          ))

      if (allowRCG is Allow)
        items :+= Item(".cg", RowKey.CodeGroup, UiText.codeGroup)

      items
    }

    val dropdownProps: DBProps =
      ButtonAndDropdown.Props.forNew(
       items       = items,
       selected    = state,
       update      = update,
      )

    @inline def render: VdomNode =
      dropdownProps.render
  }
}