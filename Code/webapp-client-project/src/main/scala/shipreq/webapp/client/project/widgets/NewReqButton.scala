package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.ReqTypes
import shipreq.webapp.client.project.feature.CreateFeature

/** A button to create a new req.
  *
  * [ New | BR ]
  */
object NewReqButton extends ButtonAndDropdown.Types[CreateFeature.RowKey] {

  type State = Option[CreateFeature.RowKey]

  @inline def initState: State =
    None

  final case class Props(state     : State,
                         reqTypes  : ReqTypes,
                         allowRCG  : Permission,
                         pw        : ProjectWidgets.AnyCtx,
                         callbacks : Option[Reusable[Callbacks]],
                         inProgress: Boolean,
                         basic     : Boolean = false,
                        ) {

    private lazy val items: NonEmptyVector[Item] = {

      var items: NonEmptyVector[Item] =
        reqTypes.liveSortedByMnemonic.map(rt =>
          Item(
            key              = rt.mnemonic.value,
            value            = CreateFeature.RowKey.req(rt.reqTypeId),
            renderInButton   = pw.reqTypeShort(rt.reqTypeId),
            renderInDropdown = pw.reqTypeFull(rt.reqTypeId),
          ))

      if (allowRCG is Allow)
        items :+= Item(".cg", CreateFeature.RowKey.CodeGroup, UiText.codeGroup)

      items
    }

    val dropdownProps: DBProps =
      ButtonAndDropdown.Props.forNew(
       items      = items,
       selected   = state,
       callbacks  = callbacks,
       inProgress = inProgress,
       basic      = basic,
      )

    @inline def render: VdomNode =
      dropdownProps.render
  }
}