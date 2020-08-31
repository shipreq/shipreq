package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses
import shipreq.base.util.{Enabled, PotentialChange}
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.CustomTagFieldGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.widgets.{Dropdown, Form}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ProjectWidgets, ReqTypeRulesEditor, ViewTags}

object TagFieldEditor {
  import DataImplicits._

  private def legalDefaultIterator(state: State, cfg: ProjectConfig): Iterator[ApplicableTagId] = {
    val tagGroupIdOption: Option[TagGroupId] =
      state match {
        case s: State.ForUpdate => Some(cfg.fields.custom(s.id).tagId)
        case s: State.ForCreate => s.tagId
      }

    tagGroupIdOption match {
      case None     => Iterator.empty
      case Some(id) =>
        val flatRows = cfg.tags.flatRowsWithRoot(id, HideDead)
        MutableArray(flatRows.iterator.map(_.tag).filterSubType[ApplicableTag])
          .sortBy(_.name)
          .iterator()
          .map(_.id)
    }
  }

  // ===================================================================================================================

  final case class Props(state     : StateSnapshot[State],
                         cfg       : ProjectConfig,
                         filterDead: FilterDead,
                         enabled   : Enabled,
                         pw        : ProjectWidgets.NoCtx,
                         router    : Routes.RouterCtl) {

    lazy val legalDefaults: ArraySeq[ApplicableTagId] =
      legalDefaultIterator(state.value, cfg).to(ArraySeq).distinct

    private lazy val legalTagGroups: Set[TagGroupId] =
      cfg.tags.liveTagGroupIds -- cfg.fields.customTagFields.iterator.map(_.tagId).filterSubType[TagGroupId]

    private[TagFieldEditor] lazy val reqTypeItems: ArraySeq[Dropdown.Item[TagGroupId]] =
      MutableArray(legalTagGroups.iterator.map(cfg.tags.needTagGroup))
        .sortBy(_.name)
        .iterator()
        .map(t => Dropdown.Item(t.id.value.toString, t.name, t.id))
        .to(ArraySeq)

    val isPossible: Boolean =
      state.value match {
        case _: State.ForUpdate => true
        case _: State.ForCreate => reqTypeItems.nonEmpty
      }

    lazy val validatorState: DataValidators.field.State =
      state.value.validatorState(cfg)

    @inline def render: VdomElement = Component(this)
  }

  // ===================================================================================================================

  sealed trait State {
    def fieldIdOption: Option[CustomField.Tag.Id]
    def tagIdOption(cfg: ProjectConfig): Option[TagGroupId]

    val reqTypeRules: ReqTypeRulesEditor.ApplicableTagDefault.State
    val derivativeTags: DerivativeTagsEditor.State

    final def validatorState(cfg: ProjectConfig): DataValidators.field.State =
      DataValidators.field.State.from(fieldIdOption, cfg)

    def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields]

    protected final def pcReqTypeRules(cfg: ProjectConfig): PotentialChange[Unit, FieldReqTypeRules.ForTagField] = {
      val ds = legalDefaultIterator(this, cfg).toSet
      PotentialChange.needFromOption(reqTypeRules.validation(cfg.reqTypes).resultWhenValid(ds))
    }

    protected final def pcDerivativeTags(cfg: ProjectConfig): PotentialChange[Unit, DerivativeTags] =
      derivativeTags.potentialChange(cfg, fieldIdOption)
  }

  object State {

    @Lenses
    final case class ForCreate(tagId         : Option[TagGroupId],
                               reqTypeRules  : ReqTypeRulesEditor.ApplicableTagDefault.State,
                               derivativeTags: DerivativeTagsEditor.State) extends State {

      override def fieldIdOption = None
      override def tagIdOption(cfg: ProjectConfig) = tagId

      override def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] = {
        val vs = validatorState(cfg)

        val pass1 =
          for {
            z <- PotentialChange.fromDisjunction(DataValidators.field.tagGroup(vs).unnamed(tagId).leftMap(_ => ()))
            a <- pcReqTypeRules(cfg)
            b <- pcDerivativeTags(cfg)
          } yield (z, a, b)

        pass1.flatMap { case (tagId, reqTypeRules, derivativeTags) =>
          val cmd = UpdateConfigCmd.CustomFieldCreateTag(tagId, reqTypeRules, derivativeTags)
          PotentialChange.Success(cmd)
        }
      }
    }

    @Lenses
    final case class ForUpdate(id            : CustomField.Tag.Id,
                               reqTypeRules  : ReqTypeRulesEditor.ApplicableTagDefault.State,
                               derivativeTags: DerivativeTagsEditor.State) extends State {

      override def fieldIdOption = Some(id)
      override def tagIdOption(cfg: ProjectConfig) = Some(cfg.fields.custom(id).tagId)

      override def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] = {
        val pass1 =
          for {
            a <- pcReqTypeRules(cfg)
            b <- pcDerivativeTags(cfg)
          } yield (a, b)

        pass1.flatMap { case (reqTypeRules, derivativeTags) =>
          val old = cfg.fields.custom(id)
          val b = CustomTagFieldGD.valueBuilder()

          b.addIfChanged(CustomTagFieldGD.FieldReqTypeRules)(old.fieldReqTypeRules, reqTypeRules)
          b.addIfChanged(CustomTagFieldGD.DerivativeTags)(old.derivativeTags, derivativeTags)

          PotentialChange.fromOption(b.nev()).map { newValues =>
            UpdateConfigCmd.CustomFieldUpdateTag(id, newValues)
          }
        }
      }
    }

    val reqTypeRules: Lens[State, ReqTypeRulesEditor.ApplicableTagDefault.State] =
      Lens[State, ReqTypeRulesEditor.ApplicableTagDefault.State](_.reqTypeRules)(r => {
        case s: ForCreate => s.copy(reqTypeRules = r)
        case s: ForUpdate => s.copy(reqTypeRules = r)
      })

    val derivativeTags: Lens[State, DerivativeTagsEditor.State] =
      Lens[State, DerivativeTagsEditor.State](_.derivativeTags)(d => {
        case s: ForCreate => s.copy(derivativeTags = d)
        case s: ForUpdate => s.copy(derivativeTags = d)
      })

    def initCreate: ForCreate =
      ForCreate(
        None,
        ReqTypeRulesEditor.State.empty,
        DerivativeTagsEditor.State.empty,
      )

    def initUpdate(id: CustomField.Tag.Id, cfg: ProjectConfig): ForUpdate = {
      val f = cfg.fields.custom(id)
      ForUpdate(
        id,
        ReqTypeRulesEditor.State.init(cfg, f.fieldReqTypeRulesByResolution),
        DerivativeTagsEditor.State.init(f.derivativeTags, f.tagId, cfg.tags),
      )
    }

    def init(id: Option[CustomField.Tag.Id], cfg: ProjectConfig): State =
      id.fold[State](initCreate)(initUpdate(_, cfg))
  }

  // ===================================================================================================================

  private implicit def tagDisplaySettings = ViewTags.DisplaySettings.tag

  private def render(p: Props): VdomNode =
    if (p.isPossible) {

      val renderDefault: ApplicableTagId ~=> VdomNode =
        Reusable.implicitly(p.pw).map(pw => id => pw.viewTags.render(id))

      val reqTypeRules =
        ReqTypeRulesEditor.ApplicableTagDefault.Component(
          ReqTypeRulesEditor.Props(
            state         = p.state.zoomStateL(State.reqTypeRules),
            reqTypes      = p.cfg.reqTypes,
            renderDefault = renderDefault,
            defaults      = p.legalDefaults,
            filterDead    = p.filterDead,
            enabled       = p.enabled))

      val derivativeTags =
        DerivativeTagsEditor.Props(
          tagGroupId = p.state.value.tagIdOption(p.cfg),
          filterDead = p.filterDead,
          cfg        = p.cfg,
          pw         = p.pw,
          state      = p.state.zoomStateL(State.derivativeTags),
        ).render

      val editors =
        TagMod(reqTypeRules, derivativeTags)

      p.state.value match {

        case _: State.ForUpdate =>
          <.div(editors)

        case s: State.ForCreate =>

          val sourceSelect =
            Dropdown.Props.Optional[TagGroupId](
              items    = p.reqTypeItems,
              selected = s.tagId.map(p.cfg.tags.needTagGroup(_).name),
              enabled  = p.enabled)(
              onChange = o => p.state.setState(s.copy(Some(o.value)))
            ).render

          val sourceField =
            Form.Field.ofEditor(sourceSelect)
              .inline
              .withLabel(FieldNames.tagFieldSource)
              .withValidated(DataValidators.field.tagGroup.unnamedFn(p.validatorState)(s.tagId))
              .withValidationUX(ValidationUX.Highlight)
              .withEnabled(p.enabled)

          <.div(Form(sourceField)(ValidationUX.Full), editors)
      }
    } else {

      // Not possible
      val msg = Shared.noSourcesMsg(FieldNames.tagFieldSource, "tag", p.router, Routes.Page.CfgTags)
      <.div(msg) // Wrap in div for tests
    }

  implicit val reusabilityStateC: Reusability[State.ForCreate] = Reusability.derive
  implicit val reusabilityStateU: Reusability[State.ForUpdate] = Reusability.derive
  implicit val reusabilityState : Reusability[State          ] = Reusability.derive
  implicit val reusabilityProps : Reusability[Props          ] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}