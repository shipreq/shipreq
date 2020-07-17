package shipreq.webapp.client.project.feature.savedview

import japgolly.scalajs.react._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview._
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
    apply(Column.Title, Live, SpecialBuiltInField.Title.name)

  def byProject(p: Project): Column => Option[ColumnPlus] = {
    val cfName = p.config.fieldName
    c => c match {
      case Column.Pubid           => Some(apply(c, Live, SpecialBuiltInField.Pubid.name))
      case Column.Code            => Some(apply(c, Live, SpecialBuiltInField.Code.name))
      case Column.Title           => Some(title)
      case Column.ReqType         => Some(apply(c, Live, SpecialBuiltInField.ReqType.name))
      case Column.AllTags         => Some(apply(c, Live, StaticField.AllTags.name))
      case Column.OtherTags       => Some(apply(c, Live, StaticField.OtherTags.name))
      case Column.Implications(d) => Some(apply(c, Live, SpecialBuiltInField.implication(d).name))
      case Column.DeletionReason  => Some(apply(c, Dead, SpecialBuiltInField.DeletionReason.name))
      case Column.CustomField(id) => p.config.fields.customFields.get(id).map(f => apply(c, f.live(p.config), cfName(id)))
    }
  }

  def byProject(p: Project, fd: FilterDead): Column => Option[ColumnPlus] =
    fd match {
      case HideDead => byProject(p).andThen(_.filter(_.live is Live))
      case ShowDead => byProject(p)
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