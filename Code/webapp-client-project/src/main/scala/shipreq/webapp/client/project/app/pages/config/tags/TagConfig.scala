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
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ErrorMsg, Optics}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.{ServerSideProcInvoker, UpdateConfigCmd}
import shipreq.webapp.client.project.widgets.{DropdownButton, ProjectWidgets, SplitScreenCrud}

object TagConfig {

  type NewState = NewTagType

  type EditorState = Unit \/ TagGroupEditor.State

  val splitScreenCrud = new SplitScreenCrud[NewState, TagId, EditorState](
    rightEmpty = SplitScreenCrud.emptyEditorMessage("tag"),
  )

  val dropdownButton = new DropdownButton.Types[NewTagType]

  final case class Props(project: ProjectConfig,
                         state  : StateSnapshot[State],
                         pw     : ProjectWidgets.NoCtx,
                         ssp    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyTags, ErrorMsg, _],
                         async  : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         ) {

    val asyncInProgress = AsyncFeature.isInProgress(async.read)

    @inline def render: VdomElement = Component(this)
  }

  sealed abstract class NewTagType(final val label: String) {
    final val item: dropdownButton.Item =
      DropdownButton.Item(label, this, label)
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

  private def editorStateLensForGroup(default: => TagGroupEditor.State): Lens[EditorState, TagGroupEditor.State] =
    Optics.disjunctionLensRight(default)

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.NewArgs

    private val initEditor: TagId => CallbackTo[EditorState] = {
      case id: TagGroupId     => $.props.map(p => \/-(TagGroupEditor.State.init(id, p.project.tags)))
      case _: ApplicableTagId => CallbackTo.pure(-\/(()))
    }

    private val updateChildren: Reusable[(TagGroupId, Vector[ApplicableTagId]) => Callback] =
      Reusable.byRef { (parent, children) =>
        for {
          p   ← $.props
          cmd = UpdateConfigCmd.TagSetApplicableChildrenOrder(parent, children)
          _   ← p.async.write.onFailureShowAndForget(p.ssp(cmd))
        } yield ()
      }

    private def newButtonProps(p: Props, args: splitScreenCrud.NewArgs): dropdownButton.DBProps =
      args match {

        case NewArgs.Disabled(sel) =>
          DropdownButton.Props.forNew[NewTagType](
            items    = NewTagType.items,
            selected = Some(sel),
            update   = None,
          )

        case NewArgs.Enabled(ss) =>
          DropdownButton.Props.forNew[NewTagType](
            items    = NewTagType.items,
            selected = Some(ss.value),
            update   = Option.unless(p.asyncInProgress)(Reusable.never(dropdownButton.Update( // TODO Reusable.never
              click  = _ => Callback.TODO,
              select = ss.setState,
            ))))
      }

    private def renderLeft(p: Props, args: splitScreenCrud.ListArgs): VdomNode =
      NonEmptySet.option(p.project.tags.topLevelIds) match {
        case Some(ids) =>
          TagTreeView.Props(
            topLevelIds    = ids,
            tags           = p.project.tags,
            filterDead     = p.state.value.filterDead,
            selected       = args.selection,
            select         = args.enabledSelect,
            projectWidgets = p.pw,
            updateChildren = updateChildren,
            enabled        = Disabled when p.asyncInProgress,
          ).render

        case None =>
          NoTags.render
      }

    private def renderEditor(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

      val subject: Option[ApplicableTagId] \/ Option[TagGroupId] =
        args.id match {
          case \/-(id: TagGroupId)      => \/-(Some(id))
          case \/-(id: ApplicableTagId) => -\/(Some(id))
          case -\/(NewTagType.TagGroup) => \/-(Option.empty[TagGroupId])
          case -\/(NewTagType.Tag)      => -\/(Option.empty[ApplicableTagId])
        }

      val editor: VdomNode =
        subject match {
          case \/-(group) =>
            val lens = editorStateLensForGroup(TagGroupEditor.State.init(group, p.project.tags))
            TagGroupEditor.Props(
              state   = args.state.zoomStateL(lens),
              project = p.project,
              pw      = p.pw,
            ).render

          case -\/(a) =>
            <.div("todo: ", a.toString)
        }

      <.div(
        editor,
        <.button("Close", ^.onClick --> args.close)
      )
    }

    def render(p: Props): VdomNode = {
      val s = p.state.value

      splitScreenCrud(
        newButton  = newButtonProps(p, _).render,
        list       = renderLeft(p, _),
        editor     = renderEditor(p, _),
        initEditor = initEditor,
        state      = p.state,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("TagConfig")
    .renderBackend[Backend]
    .build
}