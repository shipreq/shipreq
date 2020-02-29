package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.ui.semantic.Icon
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
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val groupIcon = Icon.FolderOpen.tag(*.tagTreeGroupIcon)

  private val dragHandle: Enabled => VdomTag =
    Enabled.memo(e =>
      <.div(*.tagTreeDragHandle(e), "\u2630"))

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): VdomNode = {
      import p.{tags, projectWidgets}

      val enabled: Enabled =
        Enabled.when(p.select.isDefined)

      def rowState(id: TagId): *.RowState =
        if (p.selected.exists(_ ==* id))
          *.RowState.Selected
        else if (enabled is Enabled)
          *.RowState.Enabled
        else
          *.RowState.Disabled

      def renderTags(ids: NonEmptyVector[TagId], topLevel: Boolean): VdomTagOf[html.OList] = {
        val lis = VdomArray.empty()

        // Add tag groups
        MutableArray(ids.iterator.filterSubType[TagGroupId])
          .map(tags.needTagGroup)
          .sortBy(_.name)
          .iterator
          .foreach { group =>
            val id      = group.id
            val tit     = tags.tree.need(id)
            val subtree = NonEmptyVector.option(tit.children).map(renderTags(_, topLevel = false))
            val liState = *.LIState.Group(topLevel = topLevel)

            lis += <.li(
              *.tagTreeLI(liState),
              ^.key := id.value,
              <.div(
                *.tagTreeGroup(rowState(id)),
                ^.onClick -->? p.select.map(_(id)),
                groupIcon,
                group.name),
              subtree.whenDefined,
            )
          }

        // Add tags
        var firstAfterGroup = lis.rawArray.nonEmpty
        val apTags  = ids.iterator.filterSubType[ApplicableTagId].toArray
        val canDrag = !topLevel && apTags.length > 1
        apTags.foreach { id =>

          val liState = *.LIState.Tag(
            rowState        = rowState(id),
            topLevel        = topLevel,
            firstAfterGroup = firstAfterGroup,
          )

          lis += <.li(
            *.tagTreeLI(liState),
            ^.key := id.value,
            ^.onClick -->? p.select.map(_(id)),
            TagMod.when(canDrag)(dragHandle(enabled)),
            projectWidgets.tag(id),
          )

          firstAfterGroup = false
        }

        <.ol(
          if (topLevel) *.tagTree else *.tagSubTree,
          lis)
      }

      renderTags(p.topLevelIds.toNEV, topLevel = true)
    }
  }

  val Component = ScalaComponent.builder[Props]("TagTreeView")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}