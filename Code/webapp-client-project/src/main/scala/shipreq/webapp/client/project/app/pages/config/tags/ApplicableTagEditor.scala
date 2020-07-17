package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ApplicableTagGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.AutosizeTextarea
import shipreq.webapp.base.ui.widgets.Form
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ApplicableReqTypeEditor, ColourPicker, ProjectWidgets}

private[tags] object ApplicableTagEditor {
  import DataImplicits._

  final case class Props(subject   : Option[ApplicableTagId],
                         filterDead: FilterDead,
                         state     : StateSnapshot[State],
                         project   : ProjectConfig,
                         pw        : ProjectWidgets.NoCtx,
                         enabled   : Enabled,
                        ) {

    val virtualSubjectId: ApplicableTagId =
      subject.getOrElse(ApplicableTagId(-1))

    val validatorState: DataValidators.tag.State =
      state.value.validatorState(project)

    @inline def render: VdomElement = Component(this)
  }

  final case class Source(tag: ApplicableTag, rels: TagInTree.Relations)

  @Lenses
  final case class State(source  : Option[Source],
                         key     : String,
                         desc    : String,
                         colour  : ColourPicker.State,
                         reqTypes: ApplicableReqTypeEditor.State,
                         parents : TagRelationshipEditor.State,
                        ) {

    def validatorState(p: ProjectConfig): DataValidators.tag.State =
      DataValidators.tag.State.fromConfig(source.map(_.tag.id), p)

    def updateCmd(p: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyTags] = {
      val vs = validatorState(p)

      val validated =
        DataValidators.tag.applicableTag(vs)(
          (key, desc, colour.text, (reqTypes.text, reqTypes.selected)))

      PotentialChange
        .fromDisjunction(validated.leftMap(_ => ()))
        .flatMap { case (key, desc, colour, reqTypes) =>

          val rels         = buildNewRels(source.map(_.tag.id), p.tags, parents)
          val oldParents   = source.map(s => p.tags.filterLiveParents(s.rels.parents))
          val newParents   = p.tags.filterLiveParents(rels.parents)
          val liveReqTypes = source.map(_.tag.applicableReqTypes.filterReqTypes(Live, p.reqTypes))

          val b = ApplicableTagGD.valueBuilder()
          b.addIfChangedOption(ApplicableTagGD.ApplicableReqTypes)(liveReqTypes            , reqTypes)
          b.addIfChangedOption(ApplicableTagGD.Colour            )(source.map(_.tag.colour), colour)
          b.addIfChangedOption(ApplicableTagGD.Key               )(source.map(_.tag.key   ), key)
          b.addIfChangedOption(ApplicableTagGD.Desc              )(source.map(_.tag.desc  ), desc)
          b.addIfChangedOption(ApplicableTagGD.Parents           )(oldParents              , newParents)

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
    def init(id: Option[ApplicableTagId], tags: Tags, reqTypes: ReqTypes): State =
      id.fold(initNew)(init(_, tags, reqTypes))

    def init(id: ApplicableTagId, tags: Tags, reqTypes: ReqTypes): State =
      init(tags.needApplicableTag(id), tags, reqTypes)

    def init(t: ApplicableTag, tags: Tags, reqTypes: ReqTypes): State =
      State(
        source   = Some(Source(t, tags.relations(t.id))),
        key      = t.key.value,
        desc     = t.desc.getOrElse(""),
        colour   = ColourPicker.State.init(t.colour),
        reqTypes = ApplicableReqTypeEditor.State.init(t.applicableReqTypes, reqTypes),
        parents  = TagRelationshipEditor.State.parents(t.id, tags),
      )

    def initNew: State =
      State(
        source  = None,
        key     = "",
        desc    = "",
        colour  = ColourPicker.State.init(None),
        reqTypes = ApplicableReqTypeEditor.State.empty,
        parents = TagRelationshipEditor.State.empty,
      )
  }

  private def buildNewRels(sourceId   : Option[ApplicableTagId],
                           tags       : Tags,
                           parents    : TagRelationshipEditor.State,
                          ): TagInTree.Relations = {
    val oldRels = tags.relationsOption(sourceId)
    oldRels.copy(
      parents = parents.allSet.iterator.map(id => id -> oldRels.parents.get(id).flatten).toMap,
    )
  }

  implicit val reusabilitySource: Reusability[Source] = Reusability.byRef || Reusability.derive
  implicit val reusabilityProps : Reusability[Props ] = Reusability.byRef || Reusability.derive
  implicit val reusabilityState : Reusability[State ] = Reusability.byRef || Reusability.derive

  // ===================================================================================================================

  private implicit def vux = ValidationUX.Full

  final class Backend($: BackendScope[Props, Unit]) {
    import Shared.{fakeApplicableTagId, fakeApplicableTagInTree}

    private val setParents =
      StateSnapshot.withReuse.zoomL(State.parents).prepareViaProps($)(_.state)

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
        filterDead       = p.filterDead,
        hypotheticalTags = hypotheticalTags,
        pw               = p.pw,
        state            = setParents(p.state.value),
        children         = false,
        enabled          = p.enabled,
      ).render

    def render(p: Props): VdomNode = {
      val s = p.state.value

      val keyField =
        Form.Field.text
          .withLabel("Name")
          .withState(p.state.zoomStateL(State.key))
          .withValidator(DataValidators.tag.key.unnamedFn(p.validatorState))
          .withEnabledAndAutoFocus(p.enabled)

      val colourField =
        Form.Field
          .ofEditor(ColourPicker.Props(p.state.zoomStateL(State.colour), TagPalette.forGithubPicker).render)
          .withValidated(s.colour.validated)
          .withLabel("Colour")
          .withEnabled(p.enabled)

      val descField =
        Form.Field.text
          .withEditor(AutosizeTextarea.editor)
          .withLabel("Description")
          .withState(p.state.zoomStateL(State.desc))
          .withValidator(DataValidators.tag.desc.unnamedFn(p.validatorState))
          .withEnabled(p.enabled)

      val reqTypesField =
        Form.Field.replacement(
          ApplicableReqTypeEditor.Props(
            state      = p.state.zoomStateL(State.reqTypes),
            previous   = s.source.fold(ApplicableReqTypes.empty)(_.tag.applicableReqTypes),
            reqTypes   = p.project.reqTypes,
            filterDead = p.filterDead,
            enabled    = p.enabled,
          ).render
        )

      val hypotheticalTags = pxHypotheticalTags.value()
      val parents          = tagRelationships(p, hypotheticalTags)

      <.div(
        Form(keyField, colourField, descField, reqTypesField),
        <.div(*.editorRelRow, parents))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}