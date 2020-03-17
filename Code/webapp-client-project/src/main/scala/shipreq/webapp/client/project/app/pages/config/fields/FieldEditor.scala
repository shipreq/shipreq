package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.{CustomField, CustomFieldId, FieldId, ProjectConfig}

object FieldEditor {

  sealed trait Props {
    @inline final def render: VdomElement = Component(this)
  }

  object Props {
    final case class Imp (state: StateSnapshot[State.Imp]) extends Props
    final case class Tag (state: StateSnapshot[State.Tag]) extends Props
    final case class Text(state: StateSnapshot[State.Text]) extends Props
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.derive

  sealed trait State

  object State {
    final case class Imp() extends State
    final case class Tag() extends State
    final case class Text() extends State

    def initNewImp: Imp =
      Imp()

    def initNewTag: Tag =
      Tag()

    def initNewText: Text =
      Text()

    def initImp(id: CustomField.Implication.Id): Imp =
      Imp()

    def initTag(id: CustomField.Tag.Id): Tag =
      Tag()

    def initText(id: CustomField.Text.Id): Text =
      Text()

    def init(fieldId: CustomFieldId, config: ProjectConfig): State =
      fieldId match {
        case id: CustomField.Text.Id        => initText(id)
        case id: CustomField.Tag.Id         => initTag(id)
        case id: CustomField.Implication.Id => initImp(id)
      }
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomNode = {
      <.div(p.toString)
    }
  }

  val Component = ScalaComponent.builder[Props]("FieldEditor")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}