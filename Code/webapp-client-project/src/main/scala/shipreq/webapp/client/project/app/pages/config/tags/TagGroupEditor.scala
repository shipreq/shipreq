package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import monocle.Lens
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.TagGroupGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.UpdateConfigCmd
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

    val validatorState: DataValidators.tag.State =
      state.value.validatorState(project)

    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.derive

  final case class Source(group: TagGroup, rels: TagInTree.Relations)

  @Lenses
  final case class State(source     : Option[Source],
                         name       : String,
                         exclusivity: Exclusivity,
                         desc       : String,
                         parents    : TagRelationshipEditor.State,
                         children   : TagRelationshipEditor.State,
                        ) {

    def validatorState(p: ProjectConfig): DataValidators.tag.State =
      DataValidators.tag.State.fromConfig(source.map(_.group.id), p)

    def updateCmd(p: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyTags] =
      updateCmd(validatorState(p), buildNewRels(source.map(_.group.id), p.tags, parents, children))

    def updateCmd(vs: DataValidators.tag.State,
                  rels: TagInTree.Relations): PotentialChange[Unit, UpdateConfigCmd.ToModifyTags] = {

      val validated =
        DataValidators.tag.tagGroup(vs)(
        (name, MutexChildren.when(exclusivity is Exclusive), desc))

      PotentialChange
        .fromDisjunction(validated.leftMap(_ => ()))
        .flatMap { case (name, mutexChildren, desc) =>
          val b = TagGroupGD.valueBuilder()
          b.addIfChangedOption(TagGroupGD.Name         )(source.map(_.group.name         ), name)
          b.addIfChangedOption(TagGroupGD.MutexChildren)(source.map(_.group.mutexChildren), mutexChildren)
          b.addIfChangedOption(TagGroupGD.Desc         )(source.map(_.group.desc         ), desc)
          b.addIfChangedOption(TagGroupGD.Parents      )(source.map(_.rels.parents       ), rels.parents)
          b.addIfChangedOption(TagGroupGD.Children     )(source.map(_.rels.children      ), rels.children)

          PotentialChange.fromOption(b.nev()).map { newValues =>
            source match {
              case Some(s) => UpdateConfigCmd.TagGroupUpdate(s.group.id, newValues)
              case None    => UpdateConfigCmd.TagGroupCreate(newValues)
            }
          }
        }
    }
  }

  object State {
    def init(id: Option[TagGroupId], tags: Tags): State =
      id.fold(initNew)(init(_, tags))

    def init(id: TagGroupId, tags: Tags): State =
      init(tags.needTagGroup(id), tags)

    def init(t: TagGroup, tags: Tags): State =
      State(
        source      = Some(Source(t, tags.relations(t.id))),
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

  private def buildNewRels(sourceId   : Option[TagGroupId],
                           tags       : Tags,
                           parents    : TagRelationshipEditor.State,
                           children   : TagRelationshipEditor.State,
                          ): TagInTree.Relations = {
    val oldParents = tags.parentsOption(sourceId)
    MMTree.Relations(
      parents  = parents.all.toIterator.map(id => id -> oldParents.get(id).flatten).toMap,
      children = children.groups.toVector ++ children.tags,
    )
  }

  // ===================================================================================================================

  private implicit def vux = ValidationUX.Full

  final class Backend($: BackendScope[Props, Unit]) {

    private val fakeTagGroupId =
      TagGroupId(-1)

    private val fakeTagGroupInTree =
      TagInTree(TagGroup(fakeTagGroupId, "", None, MutexChildren, Live), Vector.empty)

    private val pxSourceId: Px[Option[TagGroupId]] =
      Px.props($).map(_.subject).withReuse.autoRefresh

    private val pxTags: Px[Tags] =
      Px.props($).map(_.project.tags).withReuse.autoRefresh

    private val pxChildren: Px[TagRelationshipEditor.State] =
      Px.props($).map(_.state.value.children).withReuse.autoRefresh

    private val pxParents: Px[TagRelationshipEditor.State] =
      Px.props($).map(_.state.value.parents).withReuse.autoRefresh

    private val pxHypotheticalTags: Px[Tags] =
      for {
        sourceId <- pxSourceId
        tags     <- pxTags
        parents  <- pxParents
        children <- pxChildren
      } yield {
        val newRels = buildNewRels(sourceId, tags, parents, children)

        val newTagTree: TagTree =
          sourceId match {
            case Some(id) => MMTree.ApplyRelations.trustedApply1(tags.tree, id, newRels)
            case None     => MMTree.ApplyRelations.trustedApply1(tags.tree.add(fakeTagGroupInTree), fakeTagGroupId, newRels)
          }

        // println(("="*60) + "\n" + Tags(newTagTree).prettyPrint)
        DataProp.tags.treeStructure.assert(newTagTree)

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

      val nameField =
        Form.Field.text
          .withLabel("Name")
          .withState(p.state.zoomStateL(State.name))
          .withValidator(DataValidators.tag.name.unnamedFn(p.validatorState))
          .withAutoFocus

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
          .withValidator(DataValidators.tag.desc.unnamedFn(p.validatorState))

      val hypotheticalTags = pxHypotheticalTags.value()
      val parents          = tagRelationships(p, hypotheticalTags, children = false)
      val children         = tagRelationships(p, hypotheticalTags, children = true)

      <.div(
        Form(nameField, exclusivityField, descField),
        <.div(*.editorRelRow, parents, children))
    }
  }

  val Component = ScalaComponent.builder[Props]("TagGroupEditor")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}