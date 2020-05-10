package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.ProjectWidgets


private[tags] object TagTreeView {

  final case class Props(topLevelIds       : NonEmptySet[TagId],
                         tags              : Tags,
                         filterDead        : FilterDead,
                         selected          : Option[TagId],
                         select            : Option[TagId ~=> Callback],
                         pw                : ProjectWidgets.NoCtx,
                         updateLiveChildren: Reusable[(TagGroupId, Vector[ApplicableTagId]) => Callback],
                         enabled           : Enabled,
                         onClickAnywhere   : Option[Reusable[Callback]],
                         usage             : Usage,
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  val selected = VdomAttr.devOnly[Boolean]("data-selected")

  final class Backend($: BackendScope[Props, Unit]) {

    private val dndCache: FilterDead => TagGroupId => DragToReorderFeature[ApplicableTagId] = {

      def updateChildren(groupId: TagGroupId, tags: Vector[ApplicableTagId]): Callback =
        for {
          p <- $.props
          t  = tags.filter(p.tags.needApplicableTag(_).live is Live)
          _ <- p.updateLiveChildren(groupId, t)
        } yield ()

      FilterDead.memo { fd =>

        def getData(t: Tags, id: TagGroupId): Vector[ApplicableTagId] =
          t.directApplicableChildren(id).filter(t.tagIdFilter(fd))

        Memo { groupId =>
          DragToReorderFeature[ApplicableTagId](
            getData             = $.props.map(p => getData(p.tags, groupId)),
            updateData          = u => updateChildren(groupId, u.newOrder),
            updateUI            = $.forceUpdate,
            dragOutsideToRemove = false,
            addKeysToChildren   = false,
          )
        }
      }
    }

    def render(p: Props): VdomNode = {
      import p.{tags, pw}

      val modificationEnabled: Enabled =
        p.enabled & Enabled.when(p.select.isDefined)

      val dragInProgress: Boolean =
        DragToReorderFeature.dragInProgress()

      def rowState(id: TagId, readOnly: Boolean): *.RowState =
        if (dragInProgress)
          *.RowState.Dragging
        else if (p.selected.exists(_ ==* id))
          *.RowState.Selected
        else if (modificationEnabled is Disabled)
          *.RowState.Disabled
        else if (readOnly)
          *.RowState.ReadOnly
        else
          *.RowState.Enabled

      def renderTags(it: RecursiveTagIterator, parentLive: Live): VdomTagOf[html.OList] = {
        val readOnly = parentLive.is(Dead)
        val topLevel = it.parent.isEmpty
        val lis = VdomArray.empty()

        val dnd: DragToReorderFeature[ApplicableTagId] =
          it.parent match {
            case Some(g) => dndCache(p.filterDead)(g.id)
            case None    => DragToReorderFeature.off
          }

        // Tag groups
        it.tagGroupIterator().foreach { group =>
          val id      = group.id
          val subtree = it.nextLevelNonEmpty(group)
          val liState = *.LIState.Group(topLevel = topLevel)
          val rs      = rowState(id, readOnly)

          lis += <.li(
            *.tagTreeLI((liState, DragToReorderFeature.Status.Normal)),
            selected := (rs == *.RowState.Selected),
            ^.key := id.value,
            <.div(
              *.tagTreeGroup(rs),
              Shared.group(group),
              ^.onClick -->? p.select.filterNot(_ => readOnly).map(_(id)),
            ),
            subtree.whenDefined(renderTags(_, parentLive & group.live)),
          )
        }

        // Applicable tags
        var firstAfterGroup = lis.rawArray.nonEmpty
        val apTags          = it.applicableTagIdIterator().toIndexedSeq
        def liveApTagCount  = it.applicableTagIterator().count(_.live is Live)
        val canAnyDrag      = !topLevel && !readOnly && liveApTagCount > 1

        dnd.items(apTags).foreach { item =>
          val id  = item.data
          def tag = tags.tree.need(id).tag

          val liState = *.LIState.Tag(
            rowState        = rowState(id, readOnly),
            topLevel        = topLevel,
            firstAfterGroup = firstAfterGroup,
          )

          val select: ReactEvent => Option[Callback] =
            e => p.select.filterNot(_ => readOnly).map(_(id).asEventDefault(e).void)

          lis += <.li(
            *.tagTreeLI((liState, item.status)),
            selected := (liState.rowState == *.RowState.Selected),
            ^.key := id.value,
            ^.onClick ==>? select,
            TagMod.when(canAnyDrag)(Shared.dragHandle(item, modificationEnabled, tag.live)),
            pw.tagSimple(id, includeDesc = true),
            <.div(*.usage, p.usage.tagLink(id, p.filterDead))
          )

          firstAfterGroup = false
        }

        <.ol(
          if (topLevel) *.tagTree else *.tagSubTree,
          lis)
      }

      renderTags(tags.recursiveIterator(p.filterDead), Live)(
        p.onClickAnywhere.whenDefined(^.onClick --> _))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}