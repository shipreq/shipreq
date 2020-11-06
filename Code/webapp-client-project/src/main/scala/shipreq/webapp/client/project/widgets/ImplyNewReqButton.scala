package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.ui.semantic.{Colour, Icon}
import shipreq.webapp.client.project.app.Style.{widgets => *}
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.member.project.data._

/** A button to create a new req.
  *
  * [ Imply | New | BR ]
  */
object ImplyNewReqButton {
  import ButtonsAndDropdown.ButtonProps

  type DropdownValue = CreateFeature.RowKey

  // i.e. currently selected dropdown item
  type State = Option[DropdownValue]

  @inline def initState: State =
    None

  sealed trait Method
  object Method {
    case object New   extends Method
    case object Imply extends Method
  }

  type Click = ButtonsAndDropdown.Click[DropdownValue]

  private val middleButtonTagMod = TagMod(
    ^.borderColor := "#21ba45",
  )

  final case class Props(state      : State,
                         selectItem : Option[Reusable[DropdownValue => Callback]],
                         create     : Option[Reusable[Method => Click => Callback]],
                         reqTypes   : ReqTypes,
                         allowRCG   : Permission,
                         pw         : ProjectWidgets.AnyCtx,
                         inProgress : Boolean,
                         basic      : Boolean,
                         outerTagMod: TagMod = EmptyVdom,
                        ) {

    private val dropdownProps = {

      val implyButton = ButtonProps[DropdownValue](
        colour     = Colour.Green,
        label      = "Imply",
        icon       = Icon.ShareAlternate,
        callback   = create.map(_.map(_(Method.Imply))),
        inProgress = inProgress,
      )

      val newButton = ButtonProps.newReq(
        create.map(_.map(_(Method.New))),
        inProgress)

      ButtonsAndDropdown.Props(
        buttons            = NonEmptyVector(implyButton, newButton),
        items              = NewReqButton.dropdownItems(reqTypes, allowRCG, pw),
        selectItem         = selectItem,
        selected           = state,
        outerTagMod        = outerTagMod,
        dropdownTagMod     = *.dropdownButtonGreenDropdown(basic),
        middleButtonTagMod = middleButtonTagMod,
        basic              = basic,
      )
    }

    @inline def render: VdomNode =
      dropdownProps.render
  }
}