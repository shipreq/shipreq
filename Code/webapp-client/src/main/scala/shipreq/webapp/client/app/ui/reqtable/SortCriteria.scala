package shipreq.webapp.client.app.ui.reqtable

import scalaz.Equal

sealed abstract class SortMethod(symbol: String, desc: String) {
  val optionLabel = symbol + " " + desc
}
object SortMethod {
  // http://en.wikipedia.org/wiki/Geometric_Shapes
  private def ascSym   = "▲"
  private def descSym  = "▼"
  private def blankSym = "◌"
  private def ascTxt   = "Ascending"  // English
  private def descTxt  = "Descending" // English
  private def blankTxt = "Blanks"     // English
  @inline private def txt1(a: String) = a
  private def txt2(a: String, b: String) = a + " then " + b // English

  sealed abstract class IgnoreBlanks(symbol: String, desc: String) extends SortMethod(symbol, desc)
  case object Asc  extends IgnoreBlanks(ascSym,  txt1(ascTxt))
  case object Desc extends IgnoreBlanks(descSym, txt1(descTxt))

  sealed abstract class ConsiderBlanks(symbol: String, desc: String) extends SortMethod(symbol, desc)
  case object BlanksThenAsc  extends ConsiderBlanks(blankSym + ascSym  , txt2(blankTxt, ascTxt  ))
  case object BlanksThenDesc extends ConsiderBlanks(blankSym + descSym , txt2(blankTxt, descTxt ))
  case object AscThenBlanks  extends ConsiderBlanks(ascSym   + blankSym, txt2(ascTxt,   blankTxt))
  case object DescThenBlanks extends ConsiderBlanks(descSym  + blankSym, txt2(descTxt,  blankTxt))

  implicit val equalityI: Equal[IgnoreBlanks] = Equal.equalA
  implicit val equality : Equal[SortMethod]   = Equal.equalA

  // TODO Lazy due to https://github.com/scala-js/scala-js/issues/1490
  lazy val ignoreBlanks   = Vector[IgnoreBlanks](Asc, Desc)
  lazy val considerBlanks = Vector[ConsiderBlanks](AscThenBlanks, BlanksThenAsc, BlanksThenDesc, DescThenBlanks)

  val valuesAllowed: Column.SortInconclusive => Vector[SortMethod] = {
    case Column.ReqType => ignoreBlanks
    case Column.Code
       | Column.Desc
       | Column.CustomField(_) => considerBlanks
  }
}

sealed trait SortCriterion
object SortCriterion {
  case class Inconclusive(column: Column.SortInconclusive, method: SortMethod             ) extends SortCriterion
  case class Conclusive  (column: Column.SortConclusive,   method: SortMethod.IgnoreBlanks) extends SortCriterion
}

case class SortCriteria(init: Vector[SortCriterion.Inconclusive], last: SortCriterion.Conclusive)

object SortCriteria {

  val default =
    SortCriteria(
      Vector(
        SortCriterion.Inconclusive(Column.Code,  SortMethod.AscThenBlanks)),
      SortCriterion.Conclusive    (Column.PubId, SortMethod.Asc))
}