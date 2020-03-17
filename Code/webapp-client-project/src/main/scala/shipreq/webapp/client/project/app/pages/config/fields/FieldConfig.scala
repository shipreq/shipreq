package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import monocle.Lens
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ErrorMsg, Optics, PotentialChange}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.{GeneralTheme, Toast}
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.app.pages.root.SpecialRouterCtl
import shipreq.webapp.client.project.widgets.{ButtonAndDropdown, EditorButtons, ProjectWidgets, SplitScreenCrud}

object FieldConfig {

  type NewState = NewFieldType

  type EditorState = FieldEditor.State

  val splitScreenCrud = new SplitScreenCrud[NewState, FieldId, EditorState](
    rightEmpty = SplitScreenCrud.emptyEditorMessage("field"),
  )

  val dropdownButton = new ButtonAndDropdown.Types[NewFieldType]

  final case class Props(project: Project,
                         state  : StateSnapshot[State],
                         pw     : ProjectWidgets.NoCtx,
                         ssp    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyFields, ErrorMsg, NewEvents],
                         async  : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         toast  : Toast,
                         router : SpecialRouterCtl,
                        ) {

    val asyncInProgress: Boolean =
      AsyncFeature.isInProgress(async.read)

    val filterDeadOverride: Option[FilterDead] =
      None // TODO
//      state.value.filterDead match {
//        case ShowDead => None
//        case HideDead =>
//          state.value.right.idOption match {
//            case Some(id) if project.config.tags.tree.need(id).tag.live.is(Dead) => Some(ShowDead)
//            case _                                                               => None
//          }
//      }

    def effectiveFilterDead: FilterDead =
      filterDeadOverride.getOrElse(state.value.filterDead)

    val potentialSaveCmd: PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] =
      PotentialChange.Unchanged // TODO
//      state.value.right.editorOption match {
//        case Some(\/-(s)) => s.updateCmd(project.config)
//        case Some(-\/(s)) => s.updateCmd(project.config)
//        case None         => PotentialChange.Unchanged
//      }

    @inline def render: VdomElement = Component(this)
  }

  sealed abstract class NewFieldType(final val label: String) {
    final val item: dropdownButton.Item =
      ButtonAndDropdown.Item(label, this, label)
  }

  object NewFieldType {
    case object Tag  extends NewFieldType("Tag field")
    case object Text extends NewFieldType("Text field")
    case object Imp  extends NewFieldType("Implication field")

    implicit def univEq: UnivEq[NewFieldType] = UnivEq.derive

    val values: NonEmptyVector[NewFieldType] =
      AdtMacros.adtValues[NewFieldType].sortBy(_.label)

    val items: NonEmptyVector[dropdownButton.Item] =
      values.map(_.item)
  }

  type State = splitScreenCrud.State

  def initState: State =
    splitScreenCrud.initState(NewFieldType.Text)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

//  sealed trait EditorType
//  object EditorType {
//    final case class Dead         (tagId: FieldId)                extends EditorType
////    final case class TagGroup     (id: Option[TagGroupId])      extends EditorType
////    final case class ApplicableTag(id: Option[ApplicableFieldId]) extends EditorType
//  }

//  private def editorStateLensForApTag(default: => ApplicableTagEditor.State): Lens[EditorState, ApplicableTagEditor.State] =
//    Optics.disjunctionLensLeft(default)
//
//  private def editorStateLensForGroup(default: => TagGroupEditor.State): Lens[EditorState, TagGroupEditor.State] =
//    Optics.disjunctionLensRight(default)

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.NewArgs

    private val initEditor: (NewFieldType \/ FieldId) => CallbackTo[EditorState] = {
      case \/-(id: CustomFieldId) => $.props.map(p => FieldEditor.State.init(id, p.project.config))
      case \/-(id: StaticField)   => ???
      case -\/(NewFieldType.Imp ) => CallbackTo.pure(FieldEditor.State.initNewImp)
      case -\/(NewFieldType.Tag ) => CallbackTo.pure(FieldEditor.State.initNewTag)
      case -\/(NewFieldType.Text) => CallbackTo.pure(FieldEditor.State.initNewText)
    }

    private val updateOrder: Reusable[UpdateConfigCmd.FieldUpdateOrder => Callback] =
      Reusable.byRef { cmd =>
        for {
          p   ← $.props
          _   ← submitCmd(p, cmd, "Reordered")
        } yield ()
      }

    private def submitCmd(p          : Props,
                          cmd        : UpdateConfigCmd.ToModifyFields,
                          toastPrefix: String,
                          onSuccess  : FieldId => Callback = _ => Callback.empty): Callback =
      p.async.write.forgetFailure(
        p.ssp(cmd).flatTap {
          case \/-(n) =>
            Callback.traverseOption(n.summary.allFields.headOption)(id =>
              for {
                p2 <- $.props
                _  <- p2.toast.add(s"$toastPrefix ${p2.project.config.fieldName(id)}")
                _  <- onSuccess(id)
              } yield ()
            ).asAsyncCallback

          case -\/(e) =>
            GeneralTheme.showErrorMsg(e).asAsyncCallback
        }
      )

    private def newButtonProps(p: Props, args: splitScreenCrud.NewArgs): dropdownButton.DBProps =
      args match {

        case NewArgs.Disabled(sel) =>
          ButtonAndDropdown.Props.forNew[NewFieldType](
            items    = NewFieldType.items,
            selected = Some(sel),
            update   = None,
          )

        case a: NewArgs.Enabled[NewState] =>
          ButtonAndDropdown.Props.forNew[NewFieldType](
            items    = NewFieldType.items,
            selected = Some(a.state.value),
            update   = Option.unless(p.asyncInProgress)(Reusable.byRef(a).withValue(dropdownButton.Update(
              click  = _ => a.openEditor,
              select = a.state.setState,
            ))))
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
//        usage              = p.project.tagUsage,
        router             = p.router,
      ).render

    private def renderHeader(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

//      val ateState: Option[ApplicableTagEditor.State] =
//        p.state.value.right.editorOption.flatMap(_.swap.toOption)
//
//      val colourOverride: Option[Option[Colour]] =
//        ateState.map(_.colour.validated match {
//          case \/-(c) => c
//          case -\/(_) => None
//        })
//
//      <.h2(*.editorTitle,
//        args.id match {
//
//          case \/-(id: TagGroupId) =>
//            Shared.group(p.project.config.tags.needTagGroup(id))
//
//          case -\/(NewFieldType.TagGroup) =>
//            "New tag group"
//
//          case \/-(id: ApplicableFieldId) =>
//            var tag = p.project.config.tags.needApplicableTag(id)
//            colourOverride.foreach(c => tag = tag.copy(colour = c))
//            p.pw.tagSimple(tag, includeDesc = false)(*.editorApTagHeader)
//
//          case -\/(NewFieldType.Tag) =>
//            ateState.flatMap(s => DataValidators.hashRefKey.hashRefKey.stateless.unnamed(s.key).toOption) match {
//
//              case Some(k) =>
//                val tag = Shared.fakeApplicableTag.copy(key = k, colour = colourOverride.flatten)
//                <.span("New tag: ", p.pw.tagSimple(tag, includeDesc = false)(*.editorApTagHeader))
//
//              case None =>
//                "New tag"
//            }
//        })
      <.div("Header args: ", args.toString)
    }

    private def renderEditor(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

//      val header: VdomNode =
//        renderHeader(p, args)
//
//      val editorType: EditorType =
//        args.id match {
//          case \/-(id) if p.project.config.tags.tree.need(id).tag.live.is(Dead) => EditorType.Dead(id)
//          case \/-(id: TagGroupId)                                       => EditorType.TagGroup(Some(id))
//          case \/-(id: ApplicableFieldId)                                  => EditorType.ApplicableTag(Some(id))
//          case -\/(NewFieldType.TagGroup)                                  => EditorType.TagGroup(None)
//          case -\/(NewFieldType.Tag)                                       => EditorType.ApplicableTag(None)
//        }
//
//      def createOrUpdateButtons(idOption: Option[FieldId]): EditorButtons.Props =
//        idOption match {
//          case Some(id) =>
//            EditorButtons.Props.Update(
//              abort  = args.close,
//              delete = submitCmd(p, UpdateConfigCmd.TagDelete(id), "Deleted", _ => args.reset),
//              update = p.potentialSaveCmd.map(submitCmd(p, _, "Updated", _ => args.reset)),
//            )
//
//          case None =>
//            EditorButtons.Props.Create(
//              abort  = args.close,
//              create = p.potentialSaveCmd.toOption.map(submitCmd(p, _, "Created", args.select)),
//            )
//        }
//
//      def applicableTagEditor(idOption: Option[ApplicableFieldId], enabled: Enabled) = {
//        val lens = editorStateLensForApTag(ApplicableTagEditor.State.init(idOption, p.project.config.tags, p.project.config.reqTypes))
//        ApplicableTagEditor.Props(
//          subject    = idOption,
//          filterDead = p.effectiveFilterDead,
//          state      = args.state.zoomStateL(lens),
//          project    = p.project.config,
//          pw         = p.pw,
//          enabled    = enabled,
//        ).render
//      }
//
//      def tagGroupEditor(idOption: Option[TagGroupId], enabled: Enabled) = {
//        val lens = editorStateLensForGroup(TagGroupEditor.State.init(idOption, p.project.config.tags))
//        TagGroupEditor.Props(
//          subject    = idOption,
//          filterDead = p.effectiveFilterDead,
//          state      = args.state.zoomStateL(lens),
//          project    = p.project.config,
//          pw         = p.pw,
//          enabled    = enabled,
//        ).render
//      }
//
//      editorType match {
//        case EditorType.ApplicableTag(idOption) =>
//          val editor = applicableTagEditor(idOption, Enabled)
//          val buttons = createOrUpdateButtons(idOption).render
//          <.div(header, editor, buttons)
//
//        case EditorType.TagGroup(idOption) =>
//          val editor = tagGroupEditor(idOption, Enabled)
//          val buttons = createOrUpdateButtons(idOption).render
//          <.div(header, editor, buttons)
//
//        case EditorType.Dead(id) =>
//          val editor =
//            id match {
//              case i: ApplicableFieldId => applicableTagEditor(Some(i), Disabled)
//              case i: TagGroupId      => tagGroupEditor(Some(i), Disabled)
//            }
//
//          val buttons =
//            EditorButtons.Props.Restore(
//              abort   = args.close,
//              restore = submitCmd(p, UpdateConfigCmd.TagRestore(id), "Restored", _ => args.reset),
//            ).render
//
//          <.div(header, editor, buttons)
//      }
      <.div("Editor args: ", args.toString)
    }

    def render(p: Props): VdomNode =
      splitScreenCrud(
        filterDeadOverride = p.filterDeadOverride,
        newButton          = newButtonProps(p, _).render,
        list               = renderLeft(p, _),
        editor             = renderEditor(p, _),
        initEditor         = initEditor,
        state              = p.state,
      )
  }

  val Component = ScalaComponent.builder[Props]("FieldConfig")
    .renderBackend[Backend]
    .build
}