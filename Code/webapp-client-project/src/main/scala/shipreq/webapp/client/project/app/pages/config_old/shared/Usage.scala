package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.html
import shipreq.webapp.base.data.{FilterDead, LiveDeadStatMap, Project}
import shipreq.webapp.base.filter._

object Usage {
  type View = VdomElement
  type Show = Reusable[(FilterDead, () => Filter.Valid) => VdomTagOf[html.Anchor]]

  def Show(f: (FilterDead, () => Filter.Valid) => VdomTagOf[html.Anchor]): Show =
    Reusable.byRef(f)

  def apply[Id, Data](id          : Data => Id)
                     (stats       : Project => LiveDeadStatMap[Id, Int],
                      filterSpec  : Id => Filter.Valid,
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
            show.value(fd, () => filterSpec(id(d)))(desc)
        }
      }
    px.extract
  }
}
