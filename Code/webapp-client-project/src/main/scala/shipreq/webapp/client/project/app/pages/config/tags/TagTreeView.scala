package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
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
import shipreq.webapp.client.project.widgets.ProjectWidgets


private[tags] object TagTreeView {

  final case class Props(topLevelIds   : NonEmptySet[TagId],
                         tags          : Tags,
                         filterDead    : FilterDead,
                         selected      : Option[TagId],
                         select        : Option[TagId ~=> Callback],
                         projectWidgets: ProjectWidgets.NoCtx,
                         updateChildren: Reusable[(TagGroupId, Vector[ApplicableTagId]) => Callback],
                         enabled       : Enabled,
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {

    private val dndPerGroup: TagGroupId => DragToReorderFeature[ApplicableTagId] =
      Memo { groupId =>
        DragToReorderFeature[ApplicableTagId](
          getData             = $.props.map(_.tags.directApplicableChildren(groupId)),
          updateData          = tagOrder => $.props.flatMap(_.updateChildren(groupId, tagOrder)),
          updateUI            = $.forceUpdate,
          dragOutsideToRemove = false,
          addKeysToChildren   = false,
        )
      }

    def render(p: Props): VdomNode = {
      import p.{tags, projectWidgets}

      val modificationEnabled: Enabled =
        p.enabled & Enabled.when(p.select.isDefined)

      val dragInProgress: Boolean =
        DragToReorderFeature.dragInProgress()

      def rowState(id: TagId): *.RowState =
        if (dragInProgress)
          *.RowState.Dragging
        else if (p.selected.exists(_ ==* id))
          *.RowState.Selected
        else if (modificationEnabled is Enabled)
          *.RowState.Enabled
        else
          *.RowState.Disabled

      def renderTags(ids: NonEmptyVector[TagId], parent: Option[TagGroupId]): VdomTagOf[html.OList] = {
        val topLevel = parent.isEmpty
        val lis = VdomArray.empty()

        val dnd: DragToReorderFeature[ApplicableTagId] =
          parent match {
            case Some(id) => dndPerGroup(id)
            case None     => DragToReorderFeature.off
          }

        // Tag groups
        MutableArray(ids.iterator.filterSubType[TagGroupId])
          .map(tags.needTagGroup)
          .sortBy(_.name)
          .iterator
          .foreach { group =>
            val id      = group.id
            val tit     = tags.tree.need(id)
            val subtree = NonEmptyVector.option(tit.children).map(renderTags(_, Some(id)))
            val liState = *.LIState.Group(topLevel = topLevel)

            lis += <.li(
              *.tagTreeLI((liState, DragToReorderFeature.Status.Normal)),
              ^.key := id.value,
              <.div(
                *.tagTreeGroup(rowState(id)),
                Shared.group(group),
                ^.onClick -->? p.select.map(_(id)),
              ),
              subtree.whenDefined,
            )
          }

        // Applicable tags
        var firstAfterGroup = lis.rawArray.nonEmpty
        val apTags          = ids.iterator.filterSubType[ApplicableTagId].toArray
        val canDrag         = !topLevel && apTags.length > 1

        dnd.items(apTags).foreach { item =>
          val id = item.data

          val liState = *.LIState.Tag(
            rowState        = rowState(id),
            topLevel        = topLevel,
            firstAfterGroup = firstAfterGroup,
          )

          lis += <.li(
            *.tagTreeLI((liState, item.status)),
            ^.key := id.value,
            ^.onClick -->? p.select.map(_(id)),
            TagMod.when(canDrag)(Shared.dragHandle(item, modificationEnabled)),
            projectWidgets.tag(id),
          )

          firstAfterGroup = false
        }

        <.ol(
          if (topLevel) *.tagTree else *.tagSubTree,
          lis)
      }

      renderTags(p.topLevelIds.toNEV, None)
    }
  }

  val Component = ScalaComponent.builder[Props]("TagTreeView")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}