package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses
import scala.annotation.tailrec
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._

sealed trait SortCriterion {
  def column: Column
  def method: SortMethod
  def reverse: SortCriterion
  def rotateMethod: SortCriterion
  def isConclusive: Boolean
  final def isInconclusive = !isConclusive
}

object SortCriterion {
  import Column.{NoBlanks, HasBlanks, SortConclusive, SortInconclusive}
  import SortMethod.{ConsiderBlanks, IgnoreBlanks}

  sealed trait Inconclusive extends SortCriterion {
    override def column: SortInconclusive
    override def reverse: Inconclusive
    override def rotateMethod: Inconclusive
    override final def isConclusive = false
  }

  case class InconclusiveCB(column: SortInconclusive with HasBlanks, method: ConsiderBlanks) extends Inconclusive {
    override def reverse: InconclusiveCB = copy(method = this.method.reverse)
    override def rotateMethod: InconclusiveCB = copy(method = SortMethod.nextCB(method))
  }

  case class InconclusiveIB(column: SortInconclusive with NoBlanks,  method: IgnoreBlanks)   extends Inconclusive {
    override def reverse: InconclusiveIB = copy(method = this.method.reverse)
    override def rotateMethod: InconclusiveIB = copy(method = SortMethod.nextIB(method))
  }

  case class Conclusive(column: SortConclusive, method: IgnoreBlanks) extends SortCriterion  {
    override def reverse: Conclusive = copy(method = this.method.reverse)
    override def isConclusive = true
    override def rotateMethod: Conclusive = copy(method = SortMethod.nextIB(method))
  }

  implicit def equalityIIB: UnivEq[InconclusiveIB] = UnivEq.derive
  implicit def equalityICB: UnivEq[InconclusiveCB] = UnivEq.derive
  implicit def equalityI  : UnivEq[Inconclusive]   = UnivEq.derive
  implicit def equalityC  : UnivEq[Conclusive]     = UnivEq.derive
  implicit def equality   : UnivEq[SortCriterion]  = UnivEq.derive

  implicit val reusability: Reusability[SortCriterion] = Reusability.byRefOrEqual

  def possibilitiesICB(c: SortInconclusive with HasBlanks): NonEmptyVector[InconclusiveCB] =
    SortMethod.considerBlanks.map(InconclusiveCB(c, _))

  def possibilitiesIIB(c: SortInconclusive with NoBlanks): NonEmptyVector[InconclusiveIB] =
    SortMethod.ignoreBlanks.map(InconclusiveIB(c, _))

  def possibilitiesI(c: SortInconclusive): NonEmptyVector[Inconclusive] = c match {
    case d: SortInconclusive with HasBlanks => possibilitiesICB(d)
    case d: SortInconclusive with NoBlanks  => possibilitiesIIB(d)
  }

  // Syntax Helpers
  @inline implicit class SortCriterionExt1(private val c: SortInconclusive with HasBlanks) extends AnyVal {
    @inline def /(sm: ConsiderBlanks) = InconclusiveCB(c, sm)
  }
  @inline implicit class SortCriterionExt2(private val c: SortInconclusive with NoBlanks) extends AnyVal {
    @inline def /(sm: IgnoreBlanks) = InconclusiveIB(c, sm)
  }
  @inline implicit class SortCriterionExt3(private val c: SortConclusive) extends AnyVal {
    @inline def /(sm: IgnoreBlanks) = Conclusive(c, sm)
  }
}

import SortCriterion._

@Lenses
case class SortCriteria(init: Vector[Inconclusive], last: Conclusive) {
//  def removeColumnI(c: Column.SortInconclusive): SortCriteria =
//    copy(init = init.filterNot(_.column ==* c))
//
//  def removeColumn: Column => SortCriteria = {
//    case c: Column.SortInconclusive => removeColumnI(c)
//    case _: Column.SortConclusive   => this
//  }

  def all: NonEmptyVector[SortCriterion] =
    init ++: NonEmptyVector.one[SortCriterion](last)

  def whitelistColumns(w: Set[Column.SortInconclusive]): SortCriteria =
    copy(init = init.filter(w contains _.column))

  def reverse: SortCriteria =
    SortCriteria(init.map(_.reverse), last.reverse)

  def isOrdered(c: Column): Boolean =
    isOrdered(_ ==* c)

  def isOrdered(f: Column => Boolean): Boolean =
    f(last.column) || isOrderedI(f)

  def isOrderedI(c: Column.SortInconclusive): Boolean =
    isOrderedI(_ ==* c)

  def isOrderedI(f: Column.SortInconclusive => Boolean): Boolean =
    init.exists(_.column |> f)

  def filterColumns(f: Column => Boolean): SortCriteria = {
    val i = init.filter(s => f(s.column))
    val c = if (f(last.column)) last else SortCriteria.defaultConclusive
    SortCriteria(i, c)
  }

  /**
   * The user "wants" this column, in the context of sort criteria.
   * The user's desire has been delivered through a very limited information stream consisting only of a column,
   * (i.e. usually indicated by a click on a column name), and so without additional information to help interpret
   * our masters' desire, we apply some rules:
   *
   * 1. User wants column to be primary sort column.
   * 2. If column is already the primary sort column, user wants to change its direction.
   */
  def want(column: Column): SortCriteria = {
    import SortMethod._

    column match {
      case c: Column.SortInconclusive =>
        val newInit =
          if (init.headOption.exists(_.column ==* c)) {
            // Column(I) already primary
            init.head.rotateMethod +: init.tail
          } else init.find(_.column ==* c) match {
            case Some(existing) =>
              //  Column(I) exists but isn't primary
              existing +: init.filterNot(_ eq existing)
            case None =>
              //  Column(I) is new
              val h = c match {
                case c2: Column.HasBlanks => c2 / considerBlanks.head
                case c2: Column.NoBlanks  => c2 / ignoreBlanks  .head
              }
              h +: init
          }
        SortCriteria(newInit, last)

      case c: Column.SortConclusive =>
        val newLast =
          if (c ==* last.column) {
            if (init.isEmpty)
              // Column(C) already primary
              last.rotateMethod
            else
              // Column(C) exists but isn't primary
              last
          } else
            // Column(C) change
            c / ignoreBlanks.head
        SortCriteria(Vector.empty, newLast)
    }
  }

  def rotateSortMethod(column: Column): Option[SortCriteria] =
    column match {
      case c: Column.SortInconclusive =>
        val i = init.indexWhere(_.column ==* c)
        if (i < 0)
          None
        else
          Some(copy(init = init.updated(i, init(i).rotateMethod)))
      case c: Column.SortConclusive =>
        if (c ==* last.column)
          Some(copy(last = last.rotateMethod))
        else
          None
    }

}

object SortCriteria {
  implicit def equality: UnivEq[SortCriteria] = UnivEq.derive
  implicit val reusability: Reusability[SortCriteria] = Reusability.byRefOrEqual

  val defaultConclusive =
    Column.Pubid / SortMethod.Asc

  def byPubidOnly =
    SortCriteria(Vector.empty, defaultConclusive)

  val default = SortCriteria(
    Vector(Column.Code / SortMethod.AscThenBlanks, Column.Title / SortMethod.BlanksThenAsc),
    defaultConclusive)

  def attempt(n: Vector[SortCriterion]): Option[SortCriteria] = {
    var init = Vector.empty[Inconclusive]
    val l = n.length
    @tailrec def go(i: Int): Option[SortCriteria] =
      if (i == l)
        None
      else n(i) match {
        case c: Inconclusive =>
          init :+= c
          go(i + 1)
        case c: Conclusive =>
          Some(SortCriteria(init, c))
      }
    go(0)
  }
}