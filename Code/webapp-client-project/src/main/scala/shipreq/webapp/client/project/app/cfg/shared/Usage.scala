package shipreq.webapp.client.project.app.cfg.shared

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.extra._
import org.scalajs.dom.html
import shipreq.webapp.base.data.{FilterDead, LDStats, Project}
import shipreq.webapp.base.filter.FilterSpec

object Usage {
  type View = ReactElement
  type Show = ReusableVal[(FilterDead, () => FilterSpec) => ReactTagOf[html.Anchor]]

  def Show(f: (FilterDead, () => FilterSpec) => ReactTagOf[html.Anchor]): Show =
    ReusableVal(f)(Reusability.byRef)

  def apply[Id, Data](id          : Data => Id)
                     (stats       : Project => LDStats[Id, Int],
                      filterSpec  : Data => FilterSpec,
                      pxProject   : Px[Project],
                      pxFilterDead: Px[FilterDead],
                      pxShow      : Px[Show]): Data => View = {
    val px: Px[Data => View] =
      for {
        fd   <- pxFilterDead
        p    <- pxProject
        show <- pxShow
      } yield {
        val lookup = fd ldStatsAccessor stats(p)
        d => {
          val count = lookup(id(d))
          def desc = count // + " occurrences"
          if (count == 0)
            <.span(desc)
          else
            show.value(fd, () => filterSpec(d))(desc)
        }
      }
    px.extract
  }
}
