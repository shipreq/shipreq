package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.scalajs.react._
import japgolly.univeq.UnivEq
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.lib.DataReusability._

/**
  * @param live Is the column itself live/dead.
  *             Straightforward in all cases except [[Column.DeletionReason]] which technically
  *             isn't a dead column, but is [[Dead]] here because it's only applicable to dead rows and only makes
  *             sense being rendered when [[FilterDead]] is [[ShowDead]].
  */
final case class ColumnPlus(column: Column, live: Live, name: String) {
//  def allowWhenFD(fd: FilterDead): Boolean =
//    ColumnPlus.filterDead(fd)(this)
}

object ColumnPlus {
  implicit def univEq: UnivEq[ColumnPlus] =
    UnivEq.derive

  implicit val reusability: Reusability[ColumnPlus] =
    Reusability.byRefOrUnivEq

  val title: ColumnPlus =
    apply(Column.Title, Live, ColumnNames.title)

  def byProject(p: Project): Column => Option[ColumnPlus] = {
    val cfName = CustomField.nameP(p)
    c => c match {
      case Column.Pubid           => Some(apply(c, Live, ColumnNames.pubid))
      case Column.Code            => Some(apply(c, Live, ColumnNames.code))
      case Column.Title           => Some(title)
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

  def forceNES(f: Column => Option[ColumnPlus])(cs: NonEmptySet[Column]): NonEmptySet[ColumnPlus] =
    NonEmptySet force cs.whole.flatMap(f(_).toList)

  val filterDead: FilterDead => ColumnPlus => Boolean =
    FilterDead.memo(_.filterFnBy[ColumnPlus](_.live))

  /** All columns legally-displayable according to the FilterDead setting */
  final case class All(columns: NonEmptySet[ColumnPlus]) {
    private val map: Map[Column, ColumnPlus] =
      columns.iterator.map(c => (c.column, c)).toMap

    def apply(c: Column): Option[ColumnPlus] =
      map.get(c)

    val containsColumn: Column => Boolean =
      map.contains
  }

  object All {
    implicit def univEq: UnivEq[All] =
      UnivEq.derive

    implicit val reusability: Reusability[All] =
      Reusability.byRefOrUnivEq

    def apply(p: Project): All =
      new All(forceNES(byProject(p))(Column.all(p.config)))

    def apply(p: Project, fd: FilterDead): All =
      new All(NonEmptySet.force(apply(p).columns.whole filter filterDead(fd)))
  }
}