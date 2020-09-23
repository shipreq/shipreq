package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import scala.reflect.ClassTag
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.{GeneralTheme, Toast}
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.{ButtonAndDropdown, EditorButtons, ProjectWidgets, SplitScreenCrud}

object FieldConfig {

  type NewState = NewFieldType

  sealed trait EditorState
  object EditorState {
    final case class ImpEditor (state: ImpFieldEditor .State) extends EditorState
    final case class TagEditor (state: TagFieldEditor .State) extends EditorState
    final case class TextEditor(state: TextFieldEditor.State) extends EditorState
    case object Static                                        extends EditorState
  }

  val splitScreenCrud = new SplitScreenCrud[NewState, FieldId, EditorState]

  val dropdownButton = new ButtonAndDropdown.Types[NewFieldType]

  final case class Props(project: Project,
                         state  : StateSnapshot[State],
                         pw     : ProjectWidgets.NoCtx,
                         ssp    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyFields, ErrorMsg, NewEvents],
                         async  : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         router : Routes.RouterCtl,
                         toast  : Toast,
                         usage  : Usage,
                        ) {

    val asyncInProgress: Boolean =
      async.isInProgress

    private[FieldConfig] def lifeOf(id: FieldId): Live =
      project.config.fields.need(id).live(project.config)

    val filterDeadOverride: Option[FilterDead] =
      state.value.filterDead.overrideIfDeadOption(
        state.value.right.idOption.map(lifeOf))

    def effectiveFilterDead: FilterDead =
      filterDeadOverride.getOrElse(state.value.filterDead)

    val potentialSaveCmd: PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] =
      state.value.right.editorOption match {
        case Some(EditorState.ImpEditor (s)) => s.updateCmd(project.config)
        case Some(EditorState.TagEditor (s)) => s.updateCmd(project.config)
        case Some(EditorState.TextEditor(s)) => s.updateCmd(project.config)
        case Some(EditorState.Static)
           | None                            => PotentialChange.Unchanged
      }

    @inline def render: VdomElement = Component(this)
  }

  sealed abstract class NewFieldType(final val label: String) {
    final val item: dropdownButton.Item =
      ButtonAndDropdown.Item(label, this, label)
  }

  object NewFieldType {
    final case class Static(field: StaticField.Optional) extends NewFieldType(field.name)

    sealed abstract class Custom(label: String) extends NewFieldType(label)
    case object Tag  extends Custom("Tag field")
    case object Text extends Custom("Text field")
    case object Imp  extends Custom("Implication field")

    implicit def univEqC: UnivEq[Custom] = UnivEq.derive
    implicit def univEq: UnivEq[NewFieldType] = UnivEq.derive

    val customValues: NonEmptyVector[Custom] =
      AdtMacros.adtValues[Custom].sortBy(_.label)

    val customItems: NonEmptyVector[dropdownButton.Item] =
      customValues.map(_.item)
  }

  type State = splitScreenCrud.State

  def initState: State =
    splitScreenCrud.initState(NewFieldType.Text)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val rightEmpty =
    SplitScreenCrud.emptyEditorMessage("field")

  sealed trait EditorType
  object EditorType {
    final case class Dead    (id: CustomFieldId)                      extends EditorType
    final case class LiveImp (id: Option[CustomField.Implication.Id]) extends EditorType
    final case class LiveTag (id: Option[CustomField.Tag        .Id]) extends EditorType
    final case class LiveText(id: Option[CustomField.Text       .Id]) extends EditorType
    final case class Static  (field: StaticField)                     extends EditorType
  }

  private def editorStateLensForImp(default: => ImpFieldEditor.State): Lens[EditorState, ImpFieldEditor.State] =
    Optics.coproductLens[EditorState, ImpFieldEditor.State]({ case EditorState.ImpEditor(s) => s }, s => EditorState.ImpEditor(s), default)

  private def editorStateLensForTag(default: => TagFieldEditor.State): Lens[EditorState, TagFieldEditor.State] =
    Optics.coproductLens[EditorState, TagFieldEditor.State]({ case EditorState.TagEditor(s) => s }, s => EditorState.TagEditor(s), default)

  private def editorStateLensForText(default: => TextFieldEditor.State): Lens[EditorState, TextFieldEditor.State] =
    Optics.coproductLens[EditorState, TextFieldEditor.State]({ case EditorState.TextEditor(s) => s }, s => EditorState.TextEditor(s), default)

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.NewArgs

    private val pxProjectConfig: Px[ProjectConfig] =
      Px.props($).map(_.project.config).withReuse.autoRefresh

    private val pxNewItems: Px[NonEmptyVector[dropdownButton.Item]] =
      pxProjectConfig.map { config =>

        val unusedStaticFields =
          StaticField.optional.whole.filterNot(config.fields.includes)

        val unusedItems: Vector[dropdownButton.Item] =
          MutableArray(unusedStaticFields)
            .sortBy(_.name)
            .iterator()
            .map(NewFieldType.Static(_).item)
            .toVector

        unusedItems ++: NewFieldType.customItems
      }

    private def initEditor(project: Project, arg: NewFieldType \/ FieldId): EditorState =
      arg match {
        case \/-(id: CustomField.Implication.Id) => EditorState.ImpEditor (ImpFieldEditor.State.initUpdate(id, project.config))
        case \/-(id: CustomField.Tag        .Id) => EditorState.TagEditor (TagFieldEditor.State.initUpdate(id, project.config))
        case \/-(id: CustomField.Text       .Id) => EditorState.TextEditor(TextFieldEditor.State.init(id, project.config))
        case -\/(NewFieldType.Imp              ) => EditorState.ImpEditor (ImpFieldEditor.State.initCreate)
        case -\/(NewFieldType.Tag              ) => EditorState.TagEditor (TagFieldEditor.State.initCreate)
        case -\/(NewFieldType.Text             ) => EditorState.TextEditor(TextFieldEditor.State.empty)
        case -\/(NewFieldType.Static(_)        ) => EditorState.Static
        case \/-(_: StaticField                ) => EditorState.Static
      }

    private val updateOrder: Reusable[UpdateConfigCmd.FieldUpdateOrder => Callback] =
      Reusable.byRef { cmd =>
        for {
          p   <- $.props
          _   <- submitCmd(p, cmd, "Reordered")
        } yield ()
      }

    private def submitCmd(p          : Props,
                          cmd        : UpdateConfigCmd.ToModifyFields,
                          toastPrefix: String,
                          onSuccess  : (Project, FieldId) => Callback = (_, _) => Callback.empty): Callback =
      p.async.write.forgetFailure(
        p.ssp(cmd).flatTap {
          case \/-(n) =>
            Callback.traverseOption(n.summary.allFields.headOption) { id =>
              val project = n.project
              for {
                p2 <- $.props
                _  <- p2.toast.add(s"$toastPrefix ${project.config.fieldName(id)}")
                _  <- onSuccess(project, id)
              } yield ()
            }.asAsyncCallback

          case -\/(e) =>
            GeneralTheme.showErrorMsg(e).asAsyncCallback
        }
      )

    private def submitCmdF[F <: FieldId: ClassTag](props      : Props,
                                                   cmd        : UpdateConfigCmd.ToModifyFields,
                                                   toastPrefix: String,
                                                   onSuccess  : (Project, F) => Callback): Callback =
      submitCmd(props, cmd, toastPrefix, (p, id) => id match {
        case f: F => onSuccess(p, f)
        case _    => Callback.empty
      })

    private def newButtonProps(p: Props, args: splitScreenCrud.NewArgs): dropdownButton.DBProps = {
      val items = pxNewItems.value()

      args match {

        case NewArgs.Disabled(sel) =>
          ButtonAndDropdown.Props.newReq[NewFieldType](
            items      = items,
            selected   = Some(sel),
            selectItem = None,
            create     = None,
            inProgress = p.asyncInProgress,
          )

        case a: NewArgs.Enabled[NewState] =>

          def callback(f: NewState => Callback) =
            Option.unless(p.asyncInProgress)(Reusable.byRef(a).withValue(f))

          ButtonAndDropdown.Props.newReq[NewFieldType](
            items      = items,
            selected   = Some(a.state.value),
            selectItem = callback(a.state.setState),
            create     = callback(_ => a.openEditor),
            inProgress = p.asyncInProgress,
          )
      }
    }

    private def renderLeft(p: Props, args: splitScreenCrud.ListArgs): VdomNode =
      FieldList.Props(
        config             = p.project.config,
        filterDead         = p.effectiveFilterDead,
        selected           = args.selection,
        select             = args.enabledSelect,
        pw                 = p.pw,
        updateOrder        = updateOrder,
        enabled            = Disabled when p.asyncInProgress,
        onClickAnywhere    = args.closeEditor.filter(_ => p.potentialSaveCmd.isUnchanged),
        usage              = p.usage,
      ).render

    private def renderHeader(p: Props, args: splitScreenCrud.EditorArgs): VdomNode =
      <.h2(*.editorTitle,
        args.id match {
          case \/-(id) => p.project.config.fieldName(id)
          case -\/(t)  => "New " + t.label
        })

    private def renderEditor(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

      val header: VdomNode =
        renderHeader(p, args)

      val editorType: EditorType =
        args.id match {
          case \/-(fid: CustomFieldId) =>
            if (p.lifeOf(fid) is Dead)
              EditorType.Dead(fid)
            else fid match {
              case id: CustomField.Implication.Id => EditorType.LiveImp(Some(id))
              case id: CustomField.Tag.Id         => EditorType.LiveTag(Some(id))
              case id: CustomField.Text.Id        => EditorType.LiveText(Some(id))
            }
          case -\/(NewFieldType.Imp)       => EditorType.LiveImp(None)
          case -\/(NewFieldType.Tag)       => EditorType.LiveTag(None)
          case -\/(NewFieldType.Text)      => EditorType.LiveText(None)
          case -\/(NewFieldType.Static(f)) => EditorType.Static(f)
          case \/-(f: StaticField)         => EditorType.Static(f)
        }

      def createOrUpdateButtons(idOption: Option[CustomFieldId]): EditorButtons.Props =
        EditorButtons.createOrUpdate(args)(idOption, p.potentialSaveCmd)(
          submitCmdF[CustomFieldId](p, _, _, _), UpdateConfigCmd.CustomFieldDelete)

      def impFieldEditor(idOption: Option[CustomField.Implication.Id], enabled: Enabled) = {
        val lens = editorStateLensForImp(ImpFieldEditor.State.init(idOption, p.project.config))
        ImpFieldEditor.Props(
          state      = args.state.zoomStateL(lens),
          cfg        = p.project.config,
          filterDead = p.effectiveFilterDead,
          enabled    = enabled,
          router     = p.router,
        )
      }

      def tagFieldEditor(idOption: Option[CustomField.Tag.Id], enabled: Enabled) = {
        val lens = editorStateLensForTag(TagFieldEditor.State.init(idOption, p.project.config))
        TagFieldEditor.Props(
          state      = args.state.zoomStateL(lens),
          cfg        = p.project.config,
          filterDead = p.effectiveFilterDead,
          enabled    = enabled,
          pw         = p.pw,
          router     = p.router,
        )
      }

      def textFieldEditor(idOption: Option[CustomField.Text.Id], enabled: Enabled) = {
        val lens = editorStateLensForText(TextFieldEditor.State.init(idOption, p.project.config))
        TextFieldEditor.Props(
          state      = args.state.zoomStateL(lens),
          cfg        = p.project.config,
          filterDead = p.effectiveFilterDead,
          enabled    = enabled,
        )
      }

      editorType match {

        case EditorType.LiveImp(idOption) =>
          val editor = impFieldEditor(idOption, Enabled)
          val buttons = if (editor.isPossible) createOrUpdateButtons(idOption) else EditorButtons.cancel(args)
          <.div(header, editor.render, buttons.render)

        case EditorType.LiveTag(idOption) =>
          val editor = tagFieldEditor(idOption, Enabled)
          val buttons = if (editor.isPossible) createOrUpdateButtons(idOption) else EditorButtons.cancel(args)
          <.div(header, editor.render, buttons.render)

        case EditorType.LiveText(idOption) =>
          val editor = textFieldEditor(idOption, Enabled).render
          val buttons = createOrUpdateButtons(idOption).render
          <.div(header, editor, buttons)

        case EditorType.Dead(id) =>
          val editor =
            id match {
              case i: CustomField.Tag        .Id => tagFieldEditor (Some(i),  Disabled).render
              case i: CustomField.Text       .Id => textFieldEditor(Some(i), Disabled).render
              case i: CustomField.Implication.Id => impFieldEditor (Some(i), Disabled).render
            }
          val buttons =
            EditorButtons.restore(args)(submitCmd(p, UpdateConfigCmd.CustomFieldRestore(id), _, _)).render
          <.div(header, editor, buttons)

        case EditorType.Static(f: StaticField.Mandatory) =>
          val editor  = StaticFieldEditor.Props(f, p.project.config, p.effectiveFilterDead, p.pw).render
          val buttons = EditorButtons.close(args).render
          <.div(header, editor, buttons)

        case EditorType.Static(f: StaticField.Optional) =>
          val editor  = StaticFieldEditor.Props(f, p.project.config, p.effectiveFilterDead, p.pw).render
          val inUse   = p.project.config.fields.includes(f)
          val buttons =
            if (inUse)
              EditorButtons.remove(args)(submitCmd(p, UpdateConfigCmd.StaticFieldRemove(f), _, _))
            else
              EditorButtons.add(args)(submitCmd(p, UpdateConfigCmd.StaticFieldAdd(f), _, _))
          <.div(header, editor, buttons.render)
      }
    }

    def render(p: Props): VdomNode =
      splitScreenCrud(
        filterDeadOverride = p.filterDeadOverride,
        project            = p.project,
        newButton          = newButtonProps(p, _).render,
        list               = renderLeft(p, _),
        rightEmpty         = rightEmpty,
        editor             = renderEditor(p, _),
        initEditor         = (a, b) => Some(initEditor(a, b)),
        state              = p.state,
      )
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build
}