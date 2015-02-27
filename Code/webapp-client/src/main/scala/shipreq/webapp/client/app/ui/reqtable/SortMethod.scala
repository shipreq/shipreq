package shipreq.webapp.client.app.ui.reqtable

import scalaz.NonEmptyList
import shipreq.base.util.UnivEq

sealed trait SortMethod {
  def symbol: String
  def description: String
  val optionLabel = symbol + " " + description
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
  @inline private def txt2(a: String, b: String) = a + " then " + b // English

  /** The half of [[SortMethod]]s which sort in ascending order. */
  sealed trait AscHalf extends SortMethod

  /** The half of [[SortMethod]]s which sort in ascending order. */
  sealed trait DescHalf extends SortMethod

  sealed trait IgnoreBlanks extends SortMethod

  sealed trait ConsiderBlanks extends SortMethod

  // -------------------------------------------------------------------------------------------------------------------

  case object Asc extends IgnoreBlanks with AscHalf {
    override def symbol      = ascSym
    override def description = txt1(ascTxt)
  }

  case object Desc extends IgnoreBlanks with DescHalf {
    override def symbol      = descSym
    override def description = txt1(descTxt)
  }

  case object BlanksThenAsc  extends ConsiderBlanks with AscHalf {
    override def symbol      = blankSym + ascSym
    override def description = txt2(blankTxt, ascTxt)
  }

  case object BlanksThenDesc extends ConsiderBlanks with DescHalf {
    override def symbol      = blankSym + descSym
    override def description = txt2(blankTxt, descTxt)
  }

  case object AscThenBlanks  extends ConsiderBlanks with AscHalf {
    override def symbol      = ascSym + blankSym
    override def description = txt2(ascTxt, blankTxt)
  }

  case object DescThenBlanks extends ConsiderBlanks with DescHalf {
    override def symbol      = descSym + blankSym
    override def description = txt2(descTxt, blankTxt)
  }

  // -------------------------------------------------------------------------------------------------------------------

  @inline implicit def equalityCB: UnivEq[ConsiderBlanks] = UnivEq.force
  @inline implicit def equalityIB: UnivEq[IgnoreBlanks]   = UnivEq.force
  @inline implicit def equality  : UnivEq[SortMethod]     = UnivEq.force

  // Lazy due to initialisation order. https://github.com/scala-js/scala-js/issues/1490
  lazy val ignoreBlanks   = NonEmptyList[IgnoreBlanks](Asc, Desc)
  lazy val considerBlanks = NonEmptyList[ConsiderBlanks](AscThenBlanks, BlanksThenAsc, BlanksThenDesc, DescThenBlanks)
}
