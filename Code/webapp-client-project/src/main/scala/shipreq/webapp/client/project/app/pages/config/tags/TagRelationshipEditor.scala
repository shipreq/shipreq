package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.LeftRight
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.ui.semantic.{Button, ColourPlus, Header, Icon, Segment}
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets


object TagRelationshipEditor {

  final case class Props(tags    : Tags,
                         pw      : ProjectWidgets.NoCtx,
                         state   : StateSnapshot[State],
                         children: Boolean,
                         enabled : Enabled) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  @Lenses
  final case class State(groups: Set[TagGroupId],
                         tags  : Vector[ApplicableTagId],
                         dead  : Set[TagId],
                        ) {
    def isEmpty = groups.isEmpty && tags.isEmpty && dead.isEmpty
  }

  object State {
    def empty: State =
      apply(Set.empty, Vector.empty, Set.empty)

    def parents(id: TagId, tags: Tags): State =
      apply(tags.directParents(id), Vector.empty, Set.empty)

    def children(id: TagId, tags: Tags): State = {
      val c = tags.directChildren(id)
      apply(
        groups = c.iterator.filterSubType[TagGroupId].toSet,
        tags   = c.iterator.filterSubType[ApplicableTagId].toVector,
        dead   = Set.empty)
    }

    implicit val reusability: Reusability[State] =
      Reusability.derive
  }

  private val groupLiState =
    *.LIState.Group(topLevel = false)

  private val deleteButton =
    Button(
      tipe   = Button.Type.BasicIconOnly(Icon.Trash),
      colour = ColourPlus.Negative,
    )

  final class Backend($: BackendScope[Props, Unit]) {

    private def modState(f: State => State): Callback =
      $.props.flatMap(_.state.modState(f))

    private val dnd =
      DragToReorderFeature[ApplicableTagId](
        getData             = $.props.map(_.state.value.tags),
        updateData          = tags => modState(_.copy(tags = tags)),
        updateUI            = $.forceUpdate,
        dragOutsideToRemove = false,
        addKeysToChildren   = false,
    )

    private def deleteButton(id: TagId): VdomNode = {
      val onClick: Callback =
        $.props.map(_.state).flatMap { ss =>

          val delete: State => State =
            id match {
              case i: TagGroupId      => State.groups.modify(_ - i)
              case i: ApplicableTagId => State.tags.modify(_.filter(_ !=* i))
            }

          val actuallyDelete: Callback =
            ss.modState(State.dead.modify(_ - id) compose delete)
              .delayMs(Style.animSpeedMs)
              .toCallback

          ss.modState(State.dead.modify(_ + id), actuallyDelete)
        }
      TagRelationshipEditor.deleteButton.onClick(onClick)(*.editorRelDelete)
    }

    private val dead = ^.opacity := "0"

    private def renderTagList(p: Props): VdomNode = {
      val s         = p.state.value
      val animating = s.dead.nonEmpty
      val enabled   = p.enabled & Disabled.when(animating)
      val items     = VdomArray.empty()

      val rowState =
        if (animating)
          *.RowState.Enabled
        else if (enabled is Disabled)
          *.RowState.Disabled
        else if (dnd.dragInProgress())
          *.RowState.Dragging
        else
          *.RowState.Enabled


      // Tag groups
      MutableArray(s.groups).map(p.tags.needTagGroup).sortBy(_.name).iterator.foreach { g =>
        val id = g.id
        items += <.li(
          *.editorRelTagLI((groupLiState, DragToReorderFeature.Status.Normal)),
          ^.key := id.value,
          dead.when(s.dead.contains(id)),
          <.div(
            *.editorRelGroup(rowState),
            Shared.group(g),
            deleteButton(id),
          )
        )
      }

      // Applicable tags
      val tagLiState = *.LIState.Tag(rowState, false, false)
      dnd.items(s.tags).foreach { item =>
        val id = item.data

        items += <.li(
          *.editorRelTagLI((tagLiState, item.status)),
          ^.key := id.value,
          dead.when(s.dead.contains(id)),
          Shared.dragHandle(item, enabled),
          p.pw.tag(id),
          deleteButton(id),
        )
      }

      <.ol(*.editorRelTree, items)
    }

    def render(p: Props): VdomNode = {
      val s     = p.state.value
      val lr    = LeftRight.Right.when(p.children)
      val title = if (p.children) "Children" else "Parents"
      val tags  = if (s.isEmpty) EmptyVdom else renderTagList(p)

      val footer: VdomNode =
        <.div("BUTTON HERE")

      <.div(*.editorRelOuter(lr),
        Segment.fullHeight(
          <.div(*.editorRelInner,
            <.div(*.editorRelHeader,
              Header.h4(title)),
            <.div(*.editorRelBody, tags),
            <.div(*.editorRelFooter, footer),
          )))
    }
  }

  val Component = ScalaComponent.builder[Props]("TagRelationshipEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}