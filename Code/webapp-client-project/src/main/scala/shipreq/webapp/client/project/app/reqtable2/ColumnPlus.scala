package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.extra.Reusability
import japgolly.univeq.UnivEq
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.base.data._
import shipreq.webapp.client.base.lib.DataReusability._

/**
  * @param live Is the column itself live/dead.
  *             Straightforward in all cases except [[Column.DeletionReason]] which technically
  *             isn't a dead column, but is [[Dead]] here because it's only applicable to dead rows and only makes
  *             sense being rendered when [[FilterDead]] is [[ShowDead]].
  */
final case class ColumnPlus(column: Column, live: Live, name: String)

object ColumnPlus {

  implicit def univEq: UnivEq[ColumnPlus] =
    UnivEq.derive

  implicit val reusability: Reusability[ColumnPlus] =
    Reusability.byRefOrUnivEq

  def byProject(p: Project): Column => Option[ColumnPlus] = {
    val cfName = CustomField.nameP(p)
    c => c match {
      case Column.Pubid           => Some(apply(c, Live, ColumnNames.pubid))
      case Column.Code            => Some(apply(c, Live, ColumnNames.code))
      case Column.Title           => Some(apply(c, Live, ColumnNames.title))
      case Column.ReqType         => Some(apply(c, Live, ColumnNames.reqType))
      case Column.Tags            => Some(apply(c, Live, ColumnNames.tags))
      case Column.Implications(d) => Some(apply(c, Live, ColumnNames.implications(d)))
      case Column.DeletionReason  => Some(apply(c, Dead, ColumnNames.deletionReason))
      case Column.CustomField(id) => p.config.fields.customFields.get(id).map(f => apply(c, f.live(p.config), cfName(f)))
    }
  }

  /** Forcing is fine because
    * 1. certain built-in columns are mandatory and can't be removed
    * 2. built-in columns always have a corresponding [[ColumnPlus]] value
    */
  def forceNEV(f: Column => Option[ColumnPlus])(cs: NonEmptyVector[Column]): NonEmptyVector[ColumnPlus] =
    NonEmptyVector force cs.whole.flatMap(f(_).toList)
}