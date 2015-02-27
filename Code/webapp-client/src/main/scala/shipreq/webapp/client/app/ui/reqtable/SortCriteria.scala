package shipreq.webapp.client.app.ui.reqtable

import scalaz.NonEmptyList
import shipreq.base.util.UnivEq
import shipreq.webapp.base.TypeclassDerivation._

sealed trait SortCriterion {
  def column: Column
  def method: SortMethod
}

object SortCriterion {
  import Column.{NoBlanks, HasBlanks, SortConclusive, SortInconclusive}
  import SortMethod.{ConsiderBlanks, IgnoreBlanks}

  sealed trait Inconclusive extends SortCriterion {
    override def column: SortInconclusive
  }

  case class InconclusiveCB(column: SortInconclusive with HasBlanks, method: ConsiderBlanks) extends Inconclusive
  case class InconclusiveIB(column: SortInconclusive with NoBlanks,  method: IgnoreBlanks)   extends Inconclusive

  case class Conclusive(column: SortConclusive, method: IgnoreBlanks) extends SortCriterion

  @inline implicit def equalityIIB: UnivEq[InconclusiveIB] = deriveUnivEq
  @inline implicit def equalityICB: UnivEq[InconclusiveCB] = deriveUnivEq
  @inline implicit def equalityI  : UnivEq[Inconclusive]   = deriveUnivEq
  @inline implicit def equalityC  : UnivEq[Conclusive]     = deriveUnivEq
  @inline implicit def equality   : UnivEq[SortCriterion]  = deriveUnivEq

  def possibilitiesICB(c: SortInconclusive with HasBlanks): NonEmptyList[InconclusiveCB] =
    SortMethod.considerBlanks.map(InconclusiveCB(c, _))

  def possibilitiesIIB(c: SortInconclusive with NoBlanks): NonEmptyList[InconclusiveIB] =
    SortMethod.ignoreBlanks.map(InconclusiveIB(c, _))

  def possibilitiesI(c: SortInconclusive): NonEmptyList[Inconclusive] = c match {
    case d: SortInconclusive with HasBlanks => possibilitiesICB(d)
    case d: SortInconclusive with NoBlanks  => possibilitiesIIB(d)
  }
}

import SortCriterion._

case class SortCriteria(init: Vector[Inconclusive], last: Conclusive) {
//  def removeColumnI(c: Column.SortInconclusive): SortCriteria =
//    copy(init = init.filterNot(_.column ≟ c))
//
//  def removeColumn: Column => SortCriteria = {
//    case c: Column.SortInconclusive => removeColumnI(c)
//    case _: Column.SortConclusive   => this
//  }

  def whitelistColumns(w: Set[Column.SortInconclusive]): SortCriteria =
    copy(init = init.filter(w contains _.column))
}

object SortCriteria {

  val default =
    SortCriteria(
      Vector(
        InconclusiveCB(Column.Code,  SortMethod.AscThenBlanks)),
      Conclusive      (Column.PubId, SortMethod.Asc))
}