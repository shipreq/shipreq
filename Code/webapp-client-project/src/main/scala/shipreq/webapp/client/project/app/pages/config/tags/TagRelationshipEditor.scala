package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.{LeftRight, MMTree}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.ui.semantic.{Button, ColourPlus, Header, Icon, Segment}
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{DropdownButton, ProjectWidgets}


object TagRelationshipEditor {
  import DataImplicits._
  import TagInTree.Relations

  /**
    * @param subject In the case of new tag creation, use a negative TagId
    * @param hypotheticalTags Normal [[Tags]] but with all unsaved changes applied.
    */
  final case class Props(subject         : TagId,
                         hypotheticalTags: Tags,
                         pw              : ProjectWidgets.NoCtx,
                         state           : StateSnapshot[State],
                         children        : Boolean,
                         enabled         : Enabled) {

    private[TagRelationshipEditor] val dirCtx: DirCtx =
      DirCtx(children = children)

    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.byRef || Reusability.derive

  @Lenses
  final case class State(groups: Set[TagGroupId],
                         tags  : Vector[ApplicableTagId],
                         dead  : Set[TagId],
                        ) {

    def isEmpty: Boolean =
      groups.isEmpty && tags.isEmpty && dead.isEmpty

    val all: Set[TagId] =
      groups ++ tags
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

  private val indent: Int => TagMod =
    Memo.int { i =>
      ^.paddingLeft := s"${i * 3}ex"
    }

  private final class DirCtx(children: Boolean) {
    val title: String =
      if (children) "Children" else "Parents"

    val leftRight: LeftRight =
      LeftRight.Right.when(children)

    val addRelation: (TagId, Relations) => Relations =
      if (children)
        (child, r) => r.copy(children = r.children :+ child)
      else
        (parent, r) => r.copy(parents = r.parents.updated(parent, None))

    val addLabel: VdomNode =
      s"Add ${if (children) "child" else "parent"}..."

    val legalChild: TagId => Boolean =
      if (children)
        _ => true
      else {
        case _: TagGroupId      => true
        case _: ApplicableTagId => false
      }
  }

  private object DirCtx {
    def apply(children: Boolean): DirCtx =
      if (children) Children else Parents

    private val Parents  = new DirCtx(children = false)
    private val Children = new DirCtx(children = true)
  }

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
      val it        = p.hypotheticalTags.recursiveIterator(s.all, HideDead)

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
      it.tagGroupIterator().foreach { g =>
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
      val atags      = it.applicableTagIdIterator().toArray
      dnd.items(atags).foreach { item =>
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

    private def add(tagId: TagId): Callback =
      tagId match {
        case id: TagGroupId      => modState(State.groups.modify(_ + id))
        case id: ApplicableTagId => modState(State.tags.modify(_ :+ id))
      }

    private def renderAddButton(p: Props): VdomNode = {
      val all = p.state.value.all
      val tags = p.hypotheticalTags

      def canTagBeAdded(t: Tag): Boolean = {
        def isLive          = Tag.filterLive(t)
        def isLegal         = (t.id !=* p.subject) && p.dirCtx.legalChild(t.id)
        def notAlreadyAdded = !all.contains(t.id)

        // Example: won't result in a cyclic graph
        def isSafeToApply = {
          val before = MMTree.Relations.derive(p.subject, tags.directChildren.m)
          val after  = p.dirCtx.addRelation(t.id, before)
          val result = MMTree.ApplyRelations.safeApply1(tags.tree, p.subject)(after)
          result.isRight
        }

        isLive && isLegal && notAlreadyAdded && isSafeToApply
      }

      val flatTags =
        tags.flatRows(canTagBeAdded, FlatTag.FilterPolicy.OmitBadBranches)
          .filter(_.tag.live is Live)

      val items =
        flatTags.map { ft =>
          val enabled: Enabled =
            Enabled.when(ft.status ==* FlatTag.Status.Good)

          val display: VdomNode =
            ft.tag match {
              case g: TagGroup      => Shared.group(g)
              case t: ApplicableTag => p.pw.tag(t.id)
            }

          DropdownButton.Item(
            label = <.div(indent(ft.depth), display),
            select = Option.when(enabled is Enabled)(add(ft.tag.id))
          )
        }

      DropdownButton.Props(
        icon       = Icon.Plus,
        label      = p.dirCtx.addLabel,
        items      = items,
        enabled    = p.enabled,
      ).render
    }

    def render(p: Props): VdomNode = {
      val s         = p.state.value
      val tags      = if (s.isEmpty) EmptyVdom else renderTagList(p)
      val addButton = renderAddButton(p)

      <.div(*.editorRelOuter(p.dirCtx.leftRight),
        Segment.fullHeight(
          <.div(*.editorRelInner,
            <.div(*.editorRelHeader,
              Header.h4(p.dirCtx.title)),
            <.div(*.editorRelBody, tags),
            <.div(*.editorRelFooter, addButton))))
    }
  }

  val Component = ScalaComponent.builder[Props]("TagRelationshipEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}