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
import shipreq.webapp.base.event.ApplicableTagGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.UpdateConfigCmd
import shipreq.webapp.base.ui.AutosizeTextarea
import shipreq.webapp.base.ui.semantic.Form
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ColourPicker, ProjectWidgets}

private[tags] object ApplicableTagEditor {
  import DataImplicits._

  final case class Props(subject: Option[ApplicableTagId],
                         state  : StateSnapshot[State],
                         project: ProjectConfig,
                         pw     : ProjectWidgets.NoCtx
                        ) {

    val virtualSubjectId: ApplicableTagId =
      subject.getOrElse(ApplicableTagId(-1))

    val validatorState: DataValidators.tag.State =
      state.value.validatorState(project)

    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.derive

  final case class Source(tag: ApplicableTag, rels: TagInTree.Relations)

  @Lenses
  final case class State(source : Option[Source],
                         key    : String,
                         desc   : String,
                         colour : ColourPicker.State,
                         parents: TagRelationshipEditor.State,
                        ) {

    def validatorState(p: ProjectConfig): DataValidators.tag.State =
      DataValidators.tag.State.fromConfig(source.map(_.tag.id), p)

    def updateCmd(p: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyTags] =
      updateCmd(validatorState(p), buildNewRels(source.map(_.tag.id), p.tags, parents))

    def updateCmd(vs: DataValidators.tag.State,
                  rels: TagInTree.Relations): PotentialChange[Unit, UpdateConfigCmd.ToModifyTags] = {

      val validated =
        DataValidators.tag.applicableTag(vs)(
        (key, desc, colour.text, ApplicableReqTypes.empty))

      PotentialChange
        .fromDisjunction(validated.leftMap(_ => ()))
        .flatMap { case (key, desc, colour, reqTypes) =>
          val b = ApplicableTagGD.valueBuilder()
          b.addIfChangedOption(ApplicableTagGD.ApplicableReqTypes)(source.map(_.tag.applicableReqTypes), reqTypes)
          b.addIfChangedOption(ApplicableTagGD.Colour            )(source.map(_.tag.colour            ), colour)
          b.addIfChangedOption(ApplicableTagGD.Key               )(source.map(_.tag.key               ), key)
          b.addIfChangedOption(ApplicableTagGD.Desc              )(source.map(_.tag.desc              ), desc)
          b.addIfChangedOption(ApplicableTagGD.Parents           )(source.map(_.rels.parents          ), rels.parents)

          PotentialChange.fromOption(b.nev()).map { newValues =>
            source match {
              case Some(s) => UpdateConfigCmd.ApplicableTagUpdate(s.tag.id, newValues)
              case None    => UpdateConfigCmd.ApplicableTagCreate(newValues)
            }
          }
        }
    }
  }

  object State {
    def init(id: Option[ApplicableTagId], tags: Tags): State =
      id.fold(initNew)(init(_, tags))

    def init(id: ApplicableTagId, tags: Tags): State =
      init(tags.needApplicableTag(id), tags)

    def init(t: ApplicableTag, tags: Tags): State =
      State(
        source  = Some(Source(t, tags.relations(t.id))),
        key     = t.key.value,
        desc    = t.desc.getOrElse(""),
        colour  = ColourPicker.State.init(t.colour),
        parents = TagRelationshipEditor.State.parents(t.id, tags),
      )

    def initNew: State =
      State(
        source  = None,
        key     = "",
        desc    = "",
        colour  = ColourPicker.State.init(None),
        parents = TagRelationshipEditor.State.empty,
      )

//    implicit val reusability: Reusability[State] =
//      Reusability.derive

    val parentsR = Reusable.byRef(parents)
  }

  private def buildNewRels(sourceId   : Option[ApplicableTagId],
                           tags       : Tags,
                           parents    : TagRelationshipEditor.State,
                          ): TagInTree.Relations = {
    val oldRels = tags.relationsOption(sourceId)
    oldRels.copy(
      parents = parents.all.toIterator.map(id => id -> oldRels.parents.get(id).flatten).toMap,
    )
  }

  // ===================================================================================================================

  private implicit def vux = ValidationUX.Full

  final class Backend($: BackendScope[Props, Unit]) {
    import Shared.{fakeApplicableTagId, fakeApplicableTagInTree}

    private val pxSourceId: Px[Option[ApplicableTagId]] =
      Px.props($).map(_.subject).withReuse.autoRefresh

    private val pxTags: Px[Tags] =
      Px.props($).map(_.project.tags).withReuse.autoRefresh

    private val pxParents: Px[TagRelationshipEditor.State] =
      Px.props($).map(_.state.value.parents).withReuse.autoRefresh

    private val pxHypotheticalTags: Px[Tags] =
      for {
        sourceId <- pxSourceId
        tags     <- pxTags
        parents  <- pxParents
      } yield {
        val newRels = buildNewRels(sourceId, tags, parents)

        val newTagTree: TagTree =
          sourceId match {
            case Some(id) => MMTree.ApplyRelations.trustedApply1(tags.tree, id, newRels)
            case None     => MMTree.ApplyRelations.trustedApply1(tags.tree.add(fakeApplicableTagInTree), fakeApplicableTagId, newRels)
          }

        // println(("="*60) + "\n" + Tags(newTagTree).prettyPrint)
        DataProp.tags.treeStructure.assert(newTagTree)

        Tags(newTagTree)
      }

    private def tagRelationships(p: Props, hypotheticalTags: Tags) =
      TagRelationshipEditor.Props(
        subject          = p.virtualSubjectId,
        hypotheticalTags = hypotheticalTags,
        pw               = p.pw,
        state            = p.state.withReuse.zoomStateL(State.parentsR),
        children         = false,
        enabled          = Enabled,
      ).render

    def render(p: Props): VdomNode = {
      val s = p.state.value

      val keyField =
        Form.Field.text
          .withLabel("Name")
          .withState(p.state.zoomStateL(State.key))
          .withValidator(DataValidators.tag.key.unnamedFn(p.validatorState))
          .withAutoFocus

      val colourField =
        Form.Field
          .ofEditor(ColourPicker.Props(p.state.zoomStateL(State.colour), TagPalette.forGithubPicker).render)
          .withValidated(s.colour.validated, ValidationUX.Highlight)
          .withLabel("Colour")

      val descField =
        Form.Field.text
          .withEditor(AutosizeTextarea.editor)
          .withLabel("Description")
          .withState(p.state.zoomStateL(State.desc))
          .withValidator(DataValidators.tag.desc.unnamedFn(p.validatorState))

      val hypotheticalTags = pxHypotheticalTags.value()
      val parents          = tagRelationships(p, hypotheticalTags)

      <.div(
        Form(keyField, colourField, descField),
        <.div(*.editorRelRow, parents))
    }
  }

  val Component = ScalaComponent.builder[Props]("ApplicableTagEditor")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}