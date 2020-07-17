package shipreq.webapp.base.data.savedview

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util.OptionalBoolFn
import shipreq.webapp.base.data.savedview.ImpGraphConfig._
import shipreq.webapp.base.data.{FilterDead, Project, Req, ReqId, TagGroupId}
import shipreq.webapp.base.filter.CompiledFilter

final case class ImpGraphConfig(graphDir   : GraphDir,
                                labelFormat: LabelFormat,
                                colours    : Colours)

object ImpGraphConfig {

  val default: ImpGraphConfig =
    apply(
      graphDir    = GraphDir.TopToBottom,
      labelFormat = LabelFormat.Pubid,
      colours     = Colours.ByReqType,
    )

  def buildReqWhitelist(filterDead: FilterDead, filter: Option[CompiledFilter], p: Project): Option[Set[ReqId]] = {
    val fl = filterDead.filterFn.contramap[Req](_.live(p.config.reqTypes))
    val ff = OptionalBoolFn(filter.flatMap(_.req.value))
    val f  = fl && ff
    f.value.map(p.content.reqs.reqIterator().filter(_).map(_.id).toSet)
  }

  implicit def univEq: UnivEq[ImpGraphConfig] = UnivEq.derive

  // ===================================================================================================================

  sealed trait GraphDir

  object GraphDir {
    case object BottomToTop extends GraphDir
    case object LeftToRight extends GraphDir
    case object RightToLeft extends GraphDir
    case object TopToBottom extends GraphDir

    lazy val values = AdtMacros.adtValuesManually[GraphDir](
      BottomToTop,
      LeftToRight,
      RightToLeft,
      TopToBottom,
    )

    implicit def univEq: UnivEq[GraphDir] = UnivEq.derive
  }

  // ===================================================================================================================

  sealed trait Colours

  object Colours {
    case object ByReqType                          extends Colours
    final case class ByTag(tagGroupId: TagGroupId) extends Colours

    implicit def univEq: UnivEq[Colours] = UnivEq.derive
  }

  // ===================================================================================================================

  sealed trait LabelFormat

  object LabelFormat {
    case object Pubid         extends LabelFormat
    case object PubidAndTitle extends LabelFormat

    lazy val values = AdtMacros.adtValuesManually[LabelFormat](
      Pubid,
      PubidAndTitle,
    )

    implicit def univEq: UnivEq[LabelFormat] = UnivEq.derive
  }
}
