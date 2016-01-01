package shipreq.webapp.client.app.cfg.shared

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra.Px
import shipreq.webapp.base.data.{LDStats, Project}
import shipreq.webapp.base.filter.FilterSpec
import shipreq.webapp.client.app.ProjectSpaMain._
import shipreq.webapp.client.data.FilterDead

object Usage {
  type View = ReactElement

  def apply[Id, Data](id        : Data => Id)
                     (stats     : Project => LDStats[Id, Int],
                      filterSpec: Data => FilterSpec,
                      project   : Px[Project],
                      filterDead: Px[FilterDead],
                      routerCtl : Px[RouterCtl]): Data => View = {
    val px: Px[Data => View] =
      for {
        rc <- routerCtl
        fd <- filterDead
        p  <- project
      } yield {
        val lookup = fd ldStatsAccessor stats(p)
        d => {
          val count = lookup(id(d))
          def desc = count // + " occurrences"
          if (count == 0)
            <.span(desc)
          else {
            def showReqTable(e: ReactEvent) =
              ReqTableNextState(fd, Some(filterSpec(d))).set >>
                rc.setEH(ReqTable)(e)
            <.a(^.href := "#", ^.onClick ==> showReqTable, desc)
          }
        }
      }
    px.extract
  }
}
