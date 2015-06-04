package shipreq.webapp.client.app.ui.reqtable

import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.TypeclassDerivation._

sealed trait SortCriterion {
  def column: Column
  def method: SortMethod
  def reverse: SortCriterion
}

object SortCriterion {
  import Column.{NoBlanks, HasBlanks, SortConclusive, SortInconclusive}
  import SortMethod.{ConsiderBlanks, IgnoreBlanks}

  sealed trait Inconclusive extends SortCriterion {
    override def column: SortInconclusive
    override def reverse: Inconclusive
  }

  case class InconclusiveCB(column: SortInconclusive with HasBlanks, method: ConsiderBlanks) extends Inconclusive {
    override def reverse: InconclusiveCB = copy(method = this.method.reverse)
  }

  case class InconclusiveIB(column: SortInconclusive with NoBlanks,  method: IgnoreBlanks)   extends Inconclusive {
    override def reverse: InconclusiveIB = copy(method = this.method.reverse)
  }

  case class Conclusive(column: SortConclusive, method: IgnoreBlanks) extends SortCriterion  {
    override def reverse: Conclusive = copy(method = this.method.reverse)
  }

  @inline implicit def equalityIIB: UnivEq[InconclusiveIB] = deriveUnivEq
  @inline implicit def equalityICB: UnivEq[InconclusiveCB] = deriveUnivEq
  @inline implicit def equalityI  : UnivEq[Inconclusive]   = deriveUnivEq
  @inline implicit def equalityC  : UnivEq[Conclusive]     = deriveUnivEq
  @inline implicit def equality   : UnivEq[SortCriterion]  = deriveUnivEq

  def possibilitiesICB(c: SortInconclusive with HasBlanks): NonEmptyVector[InconclusiveCB] =
    SortMethod.considerBlanks.map(InconclusiveCB(c, _))

  def possibilitiesIIB(c: SortInconclusive with NoBlanks): NonEmptyVector[InconclusiveIB] =
    SortMethod.ignoreBlanks.map(InconclusiveIB(c, _))

  def possibilitiesI(c: SortInconclusive): NonEmptyVector[Inconclusive] = c match {
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

  def reverse: SortCriteria =
    SortCriteria(init.map(_.reverse), last.reverse)

  def isOrdered(c: Column): Boolean =
    isOrdered(_ ≟ c)

  def isOrdered(f: Column => Boolean): Boolean =
    f(last.column) || isOrderedI(f)

  def isOrderedI(c: Column.SortInconclusive): Boolean =
    isOrderedI(_ ≟ c)

  def isOrderedI(f: Column.SortInconclusive => Boolean): Boolean =
    init.exists(_.column |> f)

  def filterColumns(f: Column => Boolean): SortCriteria = {
    val i = init.filter(s => f(s.column))
    val c = if (f(last.column)) last else SortCriteria.defaultConclusive
    SortCriteria(i, c)
  }
}

object SortCriteria {
  implicit val equality: UnivEq[SortCriteria] = deriveUnivEq

  val defaultConclusive =
    Conclusive(Column.Pubid, SortMethod.Asc)

  def byPubidOnly =
    SortCriteria(Vector.empty, defaultConclusive)

  val default = SortCriteria(
    Vector(InconclusiveCB(Column.Code, SortMethod.AscThenBlanks)),
    defaultConclusive)
}