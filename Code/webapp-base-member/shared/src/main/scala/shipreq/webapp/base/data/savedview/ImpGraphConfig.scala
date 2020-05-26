package shipreq.webapp.base.data.savedview

import ImpGraphConfig._
import shipreq.base.util.OptionalBoolFn
import shipreq.webapp.base.data.{FilterDead, Project, Req, ReqId}
import shipreq.webapp.base.filter.CompiledFilter

final case class ImpGraphConfig(graphDir      : GraphDir,
                              //labelFormat   : Unit
                              //minChainLength: Int,
                                colours       : Colours)

object ImpGraphConfig {

  val default: ImpGraphConfig =
    apply(
      graphDir = GraphDir.TopToBottom,
      colours = Colours.AutoByReqType,
    )

  sealed trait GraphDir
  object GraphDir {
    case object TopToBottom extends GraphDir
    case object LeftToRight extends GraphDir
  }

  sealed trait Colours
  object Colours {
    case object AutoByReqType extends Colours
  }

  def buildReqWhitelist(filterDead: FilterDead, filter: Option[CompiledFilter], p: Project): Option[Set[ReqId]] = {
    val fl = filterDead.filterFn.contramap[Req](_.live(p.config.reqTypes))
    val ff = OptionalBoolFn(filter.flatMap(_.req.value))
    val f  = fl && ff
    f.value.map(p.content.reqs.reqIterator().filter(_).map(_.id).toSet)
  }
}
