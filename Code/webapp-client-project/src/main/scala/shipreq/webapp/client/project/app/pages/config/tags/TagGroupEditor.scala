package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
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
  import DataImplicits._

  final case class Props(subject: Option[TagGroupId],
                         state  : StateSnapshot[State],
                         project: ProjectConfig,
                         pw     : ProjectWidgets.NoCtx
                        ) {

    val virtualSubjectId: TagGroupId =
      subject.getOrElse(TagGroupId(-1))

    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.derive

  @Lenses
  final case class State(source     : Option[TagGroup],
                         name       : String,
                         exclusivity: Exclusivity,
                         desc       : String,
                         parents    : TagRelationshipEditor.State,
                         children   : TagRelationshipEditor.State,
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
        parents     = TagRelationshipEditor.State.parents(t.id, tags),
        children    = TagRelationshipEditor.State.children(t.id, tags),
      )

    def initNew: State =
      State(
        source      = None,
        name        = "",
        exclusivity = Exclusive,
        desc        = "",
        parents     = TagRelationshipEditor.State.empty,
        children    = TagRelationshipEditor.State.empty,
      )

//    implicit val reusability: Reusability[State] =
//      Reusability.derive

    val exclusive: Lens[State, On] =
      exclusivity ^<-> On.isoWhen(Exclusive)

    val parentsR = Reusable.byRef(parents)
    val childrenR = Reusable.byRef(children)
  }

  private implicit def vux = ValidationUX.Full

  final class Backend($: BackendScope[Props, Unit]) {

    private val pxSubject: Px[TagGroupId] =
      Px.props($).map(_.virtualSubjectId).withReuse.autoRefresh

    private val pxTags: Px[Tags] =
      Px.props($).map(_.project.tags).withReuse.autoRefresh

    private val pxChildren: Px[TagRelationshipEditor.State] =
      Px.props($).map(_.state.value.children).withReuse.autoRefresh

    private val pxParents: Px[TagRelationshipEditor.State] =
      Px.props($).map(_.state.value.parents).withReuse.autoRefresh

    private val pxNewRelations: Px[TagInTree.Relations] =
      for {
        subject  <- pxSubject
        tags     <- pxTags
        parents  <- pxParents
        children <- pxChildren
      } yield {
        val oldParents = tags.parents(subject)
        MMTree.Relations(
          parents  = parents.all.toIterator.map(id => id -> oldParents.get(id).flatten).toMap,
          children = children.groups.toVector ++ children.tags,
        )
      }

    private val pxHypotheticalTags: Px[Tags] =
      for {
        subject  <- pxSubject
        tags     <- pxTags
        newRels  <- pxNewRelations
      } yield {
        val newTagTree = MMTree.ApplyRelations.trustedApply1(tags.tree, subject, newRels)
        // println("===================================================================\n" + Tags(newTagTree).prettyPrint)
        Tags(newTagTree)
      }

    private val exclusivityLabel: VdomNode =
      React.Fragment(
        "Exclusive",
        <.div(
          *.segmentCheckboxSubtitle,
          "When more than one tag within this group is applied to a requirement, it will be reported as an issue.")
      )

    private def tagRelationships(p: Props, hypotheticalTags: Tags, children: Boolean) =
      TagRelationshipEditor.Props(
        subject          = p.virtualSubjectId,
        hypotheticalTags = hypotheticalTags,
        pw               = p.pw,
        state            = p.state.withReuse.zoomStateL(if (children) State.childrenR else State.parentsR),
        children         = children,
        enabled          = Enabled,
      ).render

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
        Form.Field.text
          .withLabel("Name")
          .withState(p.state.zoomStateL(State.name))
          .withValidator(DataValidators.tag.name.unnamedFn(vs))

      val exclusivityField =
        Form.Field.checkbox
          .asSegment
          .withLabel(exclusivityLabel)
          .withState(p.state.zoomStateL(State.exclusive))

      val descField =
        Form.Field.text
          .withEditor(AutosizeTextarea.editor)
          .withLabel("Description")
          .withState(p.state.zoomStateL(State.desc))
          .withValidator(DataValidators.tag.desc.unnamedFn(vs))

      val hypotheticalTags = pxHypotheticalTags.value()
      val parents          = tagRelationships(p, hypotheticalTags, children = false)
      val children         = tagRelationships(p, hypotheticalTags, children = true)

      <.div(
        header,
        Form(nameField, exclusivityField, descField),
        <.div(*.editorRelRow, parents, children)
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("TagGroupEditor")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}