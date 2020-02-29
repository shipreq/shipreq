package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import monocle.Lens
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.AutosizeTextarea
import shipreq.webapp.base.ui.semantic.Form
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

private[tags] object TagGroupEditor {

  final case class Props(state  : StateSnapshot[State],
                         project: ProjectConfig,
                         pw     : ProjectWidgets.NoCtx
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.derive

  @Lenses
  final case class State(source     : Option[TagGroup],
                         name       : String,
                         exclusivity: Exclusivity,
                         desc       : String,
                         parents    : Set[TagGroupId],
                         children   : Vector[TagId],
                        )

  object State {
    def init(id: Option[TagGroupId], tags: Tags): State =
      id.fold(initNew)(init(_, tags))

    def init(id: TagGroupId, tags: Tags): State =
      init(tags.needTagGroup(id), tags)

    def init(t: TagGroup, tags: Tags): State =
      State(
        source      = Some(t),
        name        = t.name,
        exclusivity = Exclusive.when(t.mutexChildren is MutexChildren),
        desc        = t.desc.getOrElse(""),
        parents     = tags.directParents(t.id),
        children    = tags.directChildren(t.id),
      )

    def initNew: State =
      State(
        source      = None,
        name        = "",
        exclusivity = Exclusive,
        desc        = "",
        parents     = Set.empty,
        children    = Vector.empty,
      )

//    implicit val reusability: Reusability[State] =
//      Reusability.derive

    val exclusive: Lens[State, Boolean] =
      exclusivity ^<-> Exclusive.isoWhen(true)
  }

  private def vux = ValidationUX.Full

  final class Backend($: BackendScope[Props, Unit]) {

    private val exclusivityLabel: VdomNode =
      React.Fragment(
        "Exclusive",
        <.div(
          *.segmentCheckboxSubtitle,
          "When more than one tag within this group is applied to a requirement, it will be reported as an issue.")
      )

    def render(p: Props): VdomNode = {
      val s = p.state.value

      val header =
        <.h2(
          *.editorTitle,
          s.source match {
            case Some(g) => Shared.group(g)
            case None    => "New tag group"
          },
        )

      val vs = DataValidators.tag.State.fromConfig(s.source.map(_.id), p.project)

      val nameField =
        Form.TextField.highLevel(
          lens  = State.name,
          vali  = DataValidators.tag.name.unnamedFn(vs),
          label = Some("Name"),
        )(vux)(p.state)

      val exclusivityField =
        Form.BooleanSegmentField.unvalidated(
          label = exclusivityLabel,
          lens  = State.exclusive,
        )(p.state)

      val descField =
        AutosizeTextarea.SemanticUiFormField.highLevel(
          lens  = State.desc,
          vali  = DataValidators.tag.desc.unnamedFn(vs),
          label = Some("Description"),
        )(vux)(p.state)

      <.div(
        header,
        Form(nameField, exclusivityField, descField),
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("TagGroupEditor")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}