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
import shipreq.webapp.client.project.widgets.ProjectWidgets


private[tags] object TagTreeView {

  final case class Props(topLevelIds    : NonEmptySet[TagId],
                         tags           : Tags,
                         filterDead     : FilterDead,
                         selected       : Option[TagId],
                         select         : Option[TagId ~=> Callback],
                         projectWidgets : ProjectWidgets.NoCtx,
                         updateChildren : Reusable[(TagGroupId, Vector[ApplicableTagId]) => Callback],
                         enabled        : Enabled,
                         onClickAnywhere: Option[Reusable[Callback]],
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

      def renderTags(it: RecursiveTagIterator): VdomTagOf[html.OList] = {
        val topLevel = it.parent.isEmpty
        val lis = VdomArray.empty()

        val dnd: DragToReorderFeature[ApplicableTagId] =
          it.parent match {
            case Some(g) => dndPerGroup(g.id)
            case None    => DragToReorderFeature.off
          }

        // Tag groups
        it.tagGroupIterator().foreach { group =>
          val id      = group.id
          val subtree = it.nextLevelNonEmpty(group)
          val liState = *.LIState.Group(topLevel = topLevel)

          lis += <.li(
            *.tagTreeLI((liState, DragToReorderFeature.Status.Normal)),
            ^.key := id.value,
            <.div(
              *.tagTreeGroup(rowState(id)),
              Shared.group(group),
              ^.onClick -->? p.select.map(_(id)),
            ),
            subtree.whenDefined(renderTags),
          )
        }

        // Applicable tags
        var firstAfterGroup = lis.rawArray.nonEmpty
        val apTags          = it.applicableTagIdIterator().toArray
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

      renderTags(tags.recursiveIterator(p.filterDead))(
        p.onClickAnywhere.whenDefined(^.onClick --> _))
    }
  }

  val Component = ScalaComponent.builder[Props]("TagTreeView")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}