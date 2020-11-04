package shipreq.webapp.member.data.savedview

import shipreq.webapp.member.sort.SortMethod

/** A combination of [[Column]] and [[SortMethod]], which is a single criterion in [[SortCriteria]].
  *
  * Example: "Sort titles A-to-Z with blanks last".
  */
sealed trait SortCriterion {
  def column: Column
  def method: SortMethod
  def reverse: SortCriterion
  def rotateMethod: SortCriterion
  def isConclusive: Boolean
  final def isInconclusive = !isConclusive
}

object SortCriterion {
  import Column.{SortConclusive, SortInconclusive, SortInconclusiveHasBlanks, SortInconclusiveNoBlanks}
  import SortMethod.{ConsiderBlanks, IgnoreBlanks}

  sealed trait Inconclusive extends SortCriterion {
    override def column: SortInconclusive
    override def reverse: Inconclusive
    override def rotateMethod: Inconclusive
    override final def isConclusive = false
  }

  final case class InconclusiveCB(column: SortInconclusiveHasBlanks, method: ConsiderBlanks) extends Inconclusive {
    override def reverse: InconclusiveCB = copy(method = this.method.reverse)
    override def rotateMethod: InconclusiveCB = copy(method = SortMethod.nextCB(method))
  }

  final case class InconclusiveIB(column: SortInconclusiveNoBlanks, method: IgnoreBlanks) extends Inconclusive {
    override def reverse: InconclusiveIB = copy(method = this.method.reverse)
    override def rotateMethod: InconclusiveIB = copy(method = SortMethod.nextIB(method))
  }

  final case class Conclusive(column: SortConclusive, method: IgnoreBlanks) extends SortCriterion {
    override def reverse: Conclusive = copy(method = this.method.reverse)
    override def isConclusive = true
    override def rotateMethod: Conclusive = copy(method = SortMethod.nextIB(method))
  }

  implicit def equalityIIB: UnivEq[InconclusiveIB] = UnivEq.derive
  implicit def equalityICB: UnivEq[InconclusiveCB] = UnivEq.derive
  implicit def equalityI  : UnivEq[Inconclusive]   = UnivEq.derive
  implicit def equalityC  : UnivEq[Conclusive]     = UnivEq.derive
  implicit def equality   : UnivEq[SortCriterion]  = UnivEq.derive

  def possibilitiesICB(c: SortInconclusiveHasBlanks): NonEmptyVector[InconclusiveCB] =
    SortMethod.considerBlanks.map(InconclusiveCB(c, _))

  def possibilitiesIIB(c: SortInconclusiveNoBlanks): NonEmptyVector[InconclusiveIB] =
    SortMethod.ignoreBlanks.map(InconclusiveIB(c, _))

  def possibilitiesI(c: SortInconclusive): NonEmptyVector[Inconclusive] = c match {
    case d: SortInconclusiveHasBlanks => possibilitiesICB(d)
    case d: SortInconclusiveNoBlanks  => possibilitiesIIB(d)
  }

  object SyntaxHelpers {
    @inline implicit class SortCriterionExt1b(private val c: SortInconclusiveHasBlanks) extends AnyVal {
      @inline def /(sm: ConsiderBlanks) = InconclusiveCB(c, sm)
    }
    @inline implicit class SortCriterionExt2b(private val c: SortInconclusiveNoBlanks) extends AnyVal {
      @inline def /(sm: IgnoreBlanks) = InconclusiveIB(c, sm)
    }
    @inline implicit class SortCriterionExt3(private val c: SortConclusive) extends AnyVal {
      @inline def /(sm: IgnoreBlanks) = Conclusive(c, sm)
    }
  }
}
