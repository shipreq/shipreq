package shipreq.webapp.client.home.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.UiText.EnglishStringExt
import shipreq.webapp.base.data.ProjectCatalogue
import shipreq.webapp.client.base.jsfacade.MomentJs
import shipreq.webapp.client.base.ui.semantic.{Icon, Size, Statistic, StatisticGroup}

object ProjectItem {

  type Props = ProjectCatalogue.Item

  val statGroupStyle = StatisticGroup.Style(Size.Tiny)

  def stat(i: Icon, n: Int, s: String) =
    Statistic.simple(TagMod(i.tag, " ", n), s.pluralise(n))

  def render(p: Props): ReactElement = {

    val m = MomentJs.fromInstant(p.lastUpdatedOrCreatedAt)
    println(s"AHH: ${m.formatIso8601} = ${m.formatHuman} = ${m.ago()}")

    /*
            %a.item{href: "project-index.haml.html"}
          .content.middle.aligned
            .header
              CardBoard
            .meta
              Updated&nbsp;
              %time{datetime: "2015-12-17T09:24:17Z", title: "December 17, 2015"} 4 months ago.
              .ui.statistics.tiny.right.floated

     */

    <.div(
      <.pre(p.toString),

      StatisticGroup.Props(
        statGroupStyle,
        stat(Icon.Write, p.eventCount, "change") ::
        stat(Icon.Cubes, p.reqCount, "req") :: Nil
      ).render

    )
  }

  val Component = FunctionalComponent(render)

}
