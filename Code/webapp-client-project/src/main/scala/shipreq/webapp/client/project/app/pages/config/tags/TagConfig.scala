package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import scalacss.ScalaCssReact._
import shipreq.base.util.{ErrorMsg, Optics, PotentialChange}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.{GeneralTheme, Toast}
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.{ButtonAndDropdown, EditorButtons, ProjectWidgets, SplitScreenCrud}

object TagConfig {

  type NewState = NewTagType

  type EditorState = ApplicableTagEditor.State \/ TagGroupEditor.State

  val splitScreenCrud = new SplitScreenCrud[NewState, TagId, EditorState]

  val dropdownButton = new ButtonAndDropdown.Types[NewTagType]

  final case class Props(project: Project,
                         state  : StateSnapshot[State],
                         pw     : ProjectWidgets.NoCtx,
                         ssp    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyTags, ErrorMsg, NewEvents],
                         async  : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         toast  : Toast,
                         usage  : Usage,
                        ) {

    val asyncInProgress: Boolean =
      async.isInProgress

    val filterDeadOverride: Option[FilterDead] =
      state.value.filterDead.overrideIfDeadOption(
        state.value.right.idOption.map(project.config.tags.tree.need(_).tag.live))

    def effectiveFilterDead: FilterDead =
      filterDeadOverride.getOrElse(state.value.filterDead)

    val potentialSaveCmd: PotentialChange[Unit, UpdateConfigCmd.ToModifyTags] =
      state.value.right.editorOption match {
        case Some(\/-(s)) => s.updateCmd(project.config)
        case Some(-\/(s)) => s.updateCmd(project.config)
        case None         => PotentialChange.Unchanged
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

  private val rightEmpty =
    SplitScreenCrud.emptyEditorMessage("tag")
    // shipreq.webapp.client.project.widgets.ColourTest.demo

  sealed trait EditorType
  object EditorType {
    final case class Dead         (tagId: TagId)                extends EditorType
    final case class TagGroup     (id: Option[TagGroupId])      extends EditorType
    final case class ApplicableTag(id: Option[ApplicableTagId]) extends EditorType
  }

  private def editorStateLensForApTag(default: => ApplicableTagEditor.State): Lens[EditorState, ApplicableTagEditor.State] =
    Optics.disjunctionLensLeft(default)

  private def editorStateLensForGroup(default: => TagGroupEditor.State): Lens[EditorState, TagGroupEditor.State] =
    Optics.disjunctionLensRight(default)

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.NewArgs

    private def initEditor(project: Project, arg: NewTagType \/ TagId): EditorState =
      arg match {
        case \/-(id: TagGroupId)      => \/-(TagGroupEditor.State.init(id, project.config.tags))
        case \/-(id: ApplicableTagId) => -\/(ApplicableTagEditor.State.init(id, project.config.tags, project.config.reqTypes))
        case -\/(NewTagType.TagGroup) => \/-(TagGroupEditor.State.initNew)
        case -\/(NewTagType.Tag)      => -\/(ApplicableTagEditor.State.initNew)
      }

    private val updateLiveChildren: Reusable[(TagGroupId, Vector[ApplicableTagId]) => Callback] =
      Reusable.byRef { (parent, children) =>
        for {
          p   <- $.props
          cmd = UpdateConfigCmd.TagSetLiveChildrenOrder(parent, children)
          _   <- submitCmd(p, cmd, "Reordered")
        } yield ()
      }

    private def submitCmd(p          : Props,
                          cmd        : UpdateConfigCmd.ToModifyTags,
                          toastPrefix: String,
                          onSuccess  : (Project, TagId) => Callback = (_, _) => Callback.empty): Callback =
      p.async.write.forgetFailure(
        p.ssp(cmd).flatTap {
          case \/-(n) =>
            Callback.traverseOption(n.summary.allTags.headOption)(id =>
              for {
                p2 <- $.props
                tag = p2.project.config.tags.tree.need(id).tag
                _  <- p2.toast.add(s"$toastPrefix ${tag.name}")
                _  <- onSuccess(n.project, id)
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
            items      = NewTagType.items,
            selected   = Some(sel),
            callbacks  = None,
            inProgress = p.asyncInProgress,
          )

        case a: NewArgs.Enabled[NewState] =>
          ButtonAndDropdown.Props.forNew[NewTagType](
            items      = NewTagType.items,
            selected   = Some(a.state.value),
            inProgress = p.asyncInProgress,
            callbacks  = Option.unless(p.asyncInProgress)(Reusable.byRef(a).withValue(dropdownButton.Callbacks(
              click  = _ => a.openEditor,
              select = a.state.setState,
            ))))
      }

    private def renderLeft(p: Props, args: splitScreenCrud.ListArgs): VdomNode =
      NonEmptySet.option(p.project.config.tags.topLevelIds) match {
        case Some(ids) =>

          TagTreeView.Props(
            topLevelIds        = ids,
            tags               = p.project.config.tags,
            filterDead         = p.effectiveFilterDead,
            selected           = args.selection,
            select             = args.enabledSelect,
            pw                 = p.pw,
            updateLiveChildren = updateLiveChildren,
            enabled            = Disabled when p.asyncInProgress,
            onClickAnywhere    = args.closeEditor.filter(_ => p.potentialSaveCmd.isUnchanged),
            usage              = p.usage,
          ).render

        case None =>
          NoTags.render
      }

    private def renderHeader(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

      val ateState: Option[ApplicableTagEditor.State] =
        p.state.value.right.editorOption.flatMap(_.swap.toOption)

      val colourOverride: Option[Option[Colour]] =
        ateState.map(_.colour.validated match {
          case \/-(c) => c
          case -\/(_) => None
        })

      <.h2(*.editorTitle,
        args.id match {

          case \/-(id: TagGroupId) =>
            Shared.group(p.project.config.tags.needTagGroup(id))

          case -\/(NewTagType.TagGroup) =>
            "New tag group"

          case \/-(id: ApplicableTagId) =>
            var tag = p.project.config.tags.needApplicableTag(id)
            colourOverride.foreach(c => tag = tag.copy(colour = c))
            p.pw.tagSimple(tag, includeDesc = false)(*.editorApTagHeader)

          case -\/(NewTagType.Tag) =>
            ateState.flatMap(s => DataValidators.hashRefKey.hashRefKey.stateless.unnamed(s.key).toOption) match {

              case Some(k) =>
                val tag = Shared.fakeApplicableTag.copy(key = k, colour = colourOverride.flatten)
                <.span("New tag: ", p.pw.tagSimple(tag, includeDesc = false)(*.editorApTagHeader))

              case None =>
                "New tag"
            }
        })
    }

    private def renderEditor(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

      val header: VdomNode =
        renderHeader(p, args)

      val editorType: EditorType =
        args.id match {
          case \/-(id) if p.project.config.tags.tree.need(id).tag.live.is(Dead) => EditorType.Dead(id)
          case \/-(id: TagGroupId)                                       => EditorType.TagGroup(Some(id))
          case \/-(id: ApplicableTagId)                                  => EditorType.ApplicableTag(Some(id))
          case -\/(NewTagType.TagGroup)                                  => EditorType.TagGroup(None)
          case -\/(NewTagType.Tag)                                       => EditorType.ApplicableTag(None)
        }

      def createOrUpdateButtons(idOption: Option[TagId]): EditorButtons.Props =
        EditorButtons.createOrUpdate(args)(idOption, p.potentialSaveCmd)(submitCmd(p, _, _, _), UpdateConfigCmd.TagDelete)

      def applicableTagEditor(idOption: Option[ApplicableTagId], enabled: Enabled) = {
        val lens = editorStateLensForApTag(ApplicableTagEditor.State.init(idOption, p.project.config.tags, p.project.config.reqTypes))
        ApplicableTagEditor.Props(
          subject    = idOption,
          filterDead = p.effectiveFilterDead,
          state      = args.state.zoomStateL(lens),
          project    = p.project.config,
          pw         = p.pw,
          enabled    = enabled,
        ).render
      }

      def tagGroupEditor(idOption: Option[TagGroupId], enabled: Enabled) = {
        val lens = editorStateLensForGroup(TagGroupEditor.State.init(idOption, p.project.config.tags))
        TagGroupEditor.Props(
          subject    = idOption,
          filterDead = p.effectiveFilterDead,
          state      = args.state.zoomStateL(lens),
          project    = p.project.config,
          pw         = p.pw,
          enabled    = enabled,
        ).render
      }

      editorType match {
        case EditorType.ApplicableTag(idOption) =>
          val editor = applicableTagEditor(idOption, Enabled)
          val buttons = createOrUpdateButtons(idOption).render
          <.div(header, editor, buttons)

        case EditorType.TagGroup(idOption) =>
          val editor = tagGroupEditor(idOption, Enabled)
          val buttons = createOrUpdateButtons(idOption).render
          <.div(header, editor, buttons)

        case EditorType.Dead(id) =>
          val editor =
            id match {
              case i: ApplicableTagId => applicableTagEditor(Some(i), Disabled)
              case i: TagGroupId      => tagGroupEditor(Some(i), Disabled)
            }

          val buttons =
            EditorButtons.restore(args)(submitCmd(p, UpdateConfigCmd.TagRestore(id), _, _)).render

          <.div(header, editor, buttons)
      }
    }

    def render(p: Props): VdomNode = {
      // println(("="*60) + "\n" + p.project.config.tags.prettyPrint)

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
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build
}