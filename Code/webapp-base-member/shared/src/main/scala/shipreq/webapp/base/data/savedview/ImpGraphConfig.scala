package shipreq.webapp.base.data.savedview

import ImpGraphConfig._

final case class ImpGraphConfig(graphDir      : GraphDir,
                              //labelFormat   : Unit
                              //minChainLength: Int,
                                colours       : Colours)

object ImpGraphConfig {

  val default: ImpGraphConfig =
    apply(
      graphDir = GraphDir.LeftToRight,
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
}
