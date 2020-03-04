package shipreq.webapp.client.project.app.pages.config.tags

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
import shipreq.webapp.base.protocol.{ServerSideProcInvoker, UpdateConfigCmd}
import shipreq.webapp.base.ui.{GeneralTheme, Toast}
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.widgets.{ButtonAndDropdown, ProjectWidgets, SplitScreenCrud}

object TagConfig {

  type NewState = NewTagType

  type EditorState = Unit \/ TagGroupEditor.State

  val splitScreenCrud = new SplitScreenCrud[NewState, TagId, EditorState](
    rightEmpty = SplitScreenCrud.emptyEditorMessage("tag"),
  )

  val dropdownButton = new ButtonAndDropdown.Types[NewTagType]

  final case class Props(project: ProjectConfig,
                         state  : StateSnapshot[State],
                         pw     : ProjectWidgets.NoCtx,
                         ssp    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyTags, ErrorMsg, NewEvents],
                         async  : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         toast  : Toast,
                         ) {

    val asyncInProgress: Boolean =
      AsyncFeature.isInProgress(async.read)

    val filterDeadOverride: Option[FilterDead] =
      state.value.filterDead match {
        case ShowDead => None
        case HideDead =>
          state.value.right.idOption match {
            case Some(id) if project.tags.tree.need(id).tag.live.is(Dead) => Some(ShowDead)
            case _                                                        => None
          }
      }

    def effectiveFilterDead: FilterDead =
      filterDeadOverride.getOrElse(state.value.filterDead)

    val potentialSaveCmd: PotentialChange[Unit, UpdateConfigCmd.ToModifyTags] =
      state.value.right.editorOption match {
        case Some(\/-(s))  => s.updateCmd(project)
        case Some(-\/(())) => PotentialChange.Unchanged
        case None          => PotentialChange.Unchanged
      }

    @inline def render: VdomElement = Component(this)
  }

  sealed abstract class NewTagType(final val label: String) {
    final val item: dropdownButton.Item =
      ButtonAndDropdown.Item(label, this, label)
  }

  object NewTagType {
    case object Tag      extends NewTagType("Tag")
    case object TagGroup extends NewTagType("Tag group")

    implicit def univEq: UnivEq[NewTagType] = UnivEq.derive

    val values: NonEmptyVector[NewTagType] =
      AdtMacros.adtValuesManually[NewTagType](Tag, TagGroup)

    val items: NonEmptyVector[dropdownButton.Item] =
      values.map(_.item)
  }

  type State = splitScreenCrud.State

  def initState: State =
    splitScreenCrud.initState(NewTagType.Tag)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait EditorType
  object EditorType {
    final case class Dead         (tagId: TagId)                extends EditorType
    final case class TagGroup     (id: Option[TagGroupId])      extends EditorType
    final case class ApplicableTag(id: Option[ApplicableTagId]) extends EditorType
  }

  private def editorStateLensForGroup(default: => TagGroupEditor.State): Lens[EditorState, TagGroupEditor.State] =
    Optics.disjunctionLensRight(default)

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.NewArgs

    private val initEditor: (NewTagType \/ TagId) => CallbackTo[EditorState] = {
      case \/-(id: TagGroupId)      => $.props.map(p => \/-(TagGroupEditor.State.init(id, p.project.tags)))
      case \/-(_: ApplicableTagId)  => CallbackTo.pure(-\/(()))
      case -\/(NewTagType.TagGroup) => CallbackTo.pure(\/-(TagGroupEditor.State.initNew))
      case -\/(NewTagType.Tag)      => CallbackTo.pure(-\/(()))
    }

    private val updateChildren: Reusable[(TagGroupId, Vector[ApplicableTagId]) => Callback] =
      Reusable.byRef { (parent, children) =>
        for {
          p   ← $.props
          cmd = UpdateConfigCmd.TagSetApplicableChildrenOrder(parent, children)
          _   ← p.async.write.onFailureShowAndForget(p.ssp(cmd))
        } yield ()
      }

    private def submitCmd(p          : Props,
                          cmd        : UpdateConfigCmd.ToModifyTags,
                          toastPrefix: String,
                          onSuccess  : TagId => Callback = _ => Callback.empty): Callback =
      p.async.write.forgetFailure(
        p.ssp(cmd).flatTap {
          case \/-(n) =>
            Callback.traverseOption(n.summary.allTags.headOption)(id =>
              for {
                p2 <- $.props
                tag = p2.project.tags.tree.need(id).tag
                _  <- p2.toast.add(s"$toastPrefix ${tag.name}")
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
          ButtonAndDropdown.Props.forNew[NewTagType](
            items    = NewTagType.items,
            selected = Some(sel),
            update   = None,
          )

        case a: NewArgs.Enabled[NewState] =>
          ButtonAndDropdown.Props.forNew[NewTagType](
            items    = NewTagType.items,
            selected = Some(a.state.value),
            update   = Option.unless(p.asyncInProgress)(Reusable.never(dropdownButton.Update( // TODO Reusable.never
              click  = _ => a.openEditor,
              select = a.state.setState,
            ))))
      }

    private def renderLeft(p: Props, args: splitScreenCrud.ListArgs): VdomNode =
      NonEmptySet.option(p.project.tags.topLevelIds) match {
        case Some(ids) =>

          TagTreeView.Props(
            topLevelIds     = ids,
            tags            = p.project.tags,
            filterDead      = p.effectiveFilterDead,
            selected        = args.selection,
            select          = args.enabledSelect,
            pw              = p.pw,
            updateChildren  = updateChildren,
            enabled         = Disabled when p.asyncInProgress,
            onClickAnywhere = args.closeEditor.filter(_ => p.potentialSaveCmd.isUnchanged),
          ).render

        case None =>
          NoTags.render
      }

    private def renderEditor(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

      val header: VdomNode =
        <.h2(
          *.editorTitle,
          args.id match {
            case \/-(id: TagGroupId)      => Shared.group(p.project.tags.needTagGroup(id))
            case \/-(id: ApplicableTagId) => p.pw.tagSimple(id, includeDesc = false)
            case -\/(NewTagType.TagGroup) => "New tag group"
            case -\/(NewTagType.Tag)      => "New tag"
          })

      val editorType: EditorType =
        args.id match {
          case \/-(id) if p.project.tags.tree.need(id).tag.live.is(Dead) => EditorType.Dead(id)
          case \/-(id: TagGroupId)                                       => EditorType.TagGroup(Some(id))
          case \/-(id: ApplicableTagId)                                  => EditorType.ApplicableTag(Some(id))
          case -\/(NewTagType.TagGroup)                                  => EditorType.TagGroup(None)
          case -\/(NewTagType.Tag)                                       => EditorType.ApplicableTag(None)
        }

      def createOrUpdateButtons(idOption: Option[TagId]): EditorButtons.Props =
        idOption match {
          case Some(id) =>
            EditorButtons.Props.Update(
              abort  = args.close,
              delete = submitCmd(p, UpdateConfigCmd.TagDelete(id), "Deleted", _ => args.close),
              update = p.potentialSaveCmd.map(submitCmd(p, _, "Updated", _ => args.reset)),
            )

          case None =>
            EditorButtons.Props.Create(
              abort  = args.close,
              create = p.potentialSaveCmd.toOption.map(submitCmd(p, _, "Created", args.select)),
            )
        }

      editorType match {
        case EditorType.ApplicableTag(id) =>
          <.div("todo")

        case EditorType.TagGroup(idOption) =>
          val lens = editorStateLensForGroup(TagGroupEditor.State.init(idOption, p.project.tags))

          val editor =
            TagGroupEditor.Props(
              subject = idOption,
              state   = args.state.zoomStateL(lens),
              project = p.project,
              pw      = p.pw,
            ).render

          val buttons = createOrUpdateButtons(idOption).render

          <.div(header, editor, buttons)

        case EditorType.Dead(id) =>
          val buttons =
            EditorButtons.Props.Restore(
              abort   = args.close,
              restore = submitCmd(p, UpdateConfigCmd.TagRestore(id), "Restored"),
            ).render

          <.div(header, buttons)
      }
    }

    def render(p: Props): VdomNode = {
      // println(("="*60) + "\n" + p.project.tags.prettyPrint)

      splitScreenCrud(
        filterDeadOverride = p.filterDeadOverride,
        newButton          = newButtonProps(p, _).render,
        list               = renderLeft(p, _),
        editor             = renderEditor(p, _),
        initEditor         = initEditor,
        state              = p.state,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("TagConfig")
    .renderBackend[Backend]
    .build
}