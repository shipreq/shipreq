package shipreq.webapp.member.sort

import shipreq.base.util.Util
/** The method by which data should be sorted.
  *
  * Basically just an enum of {Ascending,Descending} that also takes blanks into consideration.
  */
sealed trait SortMethod {
  def symbol: String
  def description: String
  val optionLabel = symbol + " " + description
  type ReverseHalf <: SortMethod
  type BlankSpec <: SortMethod
  def reverse: BlankSpec with ReverseHalf

  def ascending: Boolean
  final def descending = !ascending
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
  sealed trait AscHalf extends SortMethod {
    final override type ReverseHalf = DescHalf
    final override def ascending = true
  }

  /** The half of [[SortMethod]]s which sort in ascending order. */
  sealed trait DescHalf extends SortMethod {
    final override type ReverseHalf = AscHalf
    final override def ascending = false
  }

  sealed trait IgnoreBlanks extends SortMethod {
    final override type BlankSpec = IgnoreBlanks
  }

  sealed trait ConsiderBlanks extends SortMethod {
    final override type BlankSpec = ConsiderBlanks
  }

  // -------------------------------------------------------------------------------------------------------------------

  case object Asc extends IgnoreBlanks with AscHalf {
    override def symbol      = ascSym
    override def description = txt1(ascTxt)
    override def reverse     = Desc
  }

  case object Desc extends IgnoreBlanks with DescHalf {
    override def symbol      = descSym
    override def description = txt1(descTxt)
    override def reverse     = Asc
  }

  case object BlanksThenAsc  extends ConsiderBlanks with AscHalf {
    override def symbol      = blankSym + ascSym
    override def description = txt2(blankTxt, ascTxt)
    override def reverse     = DescThenBlanks
  }

  case object BlanksThenDesc extends ConsiderBlanks with DescHalf {
    override def symbol      = blankSym + descSym
    override def description = txt2(blankTxt, descTxt)
    override def reverse     = AscThenBlanks
  }

  case object AscThenBlanks  extends ConsiderBlanks with AscHalf {
    override def symbol      = ascSym + blankSym
    override def description = txt2(ascTxt, blankTxt)
    override def reverse     = BlanksThenDesc
  }

  case object DescThenBlanks extends ConsiderBlanks with DescHalf {
    override def symbol      = descSym + blankSym
    override def description = txt2(descTxt, blankTxt)
    override def reverse     = BlanksThenAsc
  }

  // -------------------------------------------------------------------------------------------------------------------

  @inline implicit def equalityCB: UnivEq[ConsiderBlanks] = UnivEq.derive
  @inline implicit def equalityIB: UnivEq[IgnoreBlanks]   = UnivEq.derive
  @inline implicit def equality  : UnivEq[SortMethod]     = UnivEq.derive

  // Lazy due to initialisation order. https://github.com/scala-js/scala-js/issues/1490
  lazy val ignoreBlanks   = NonEmptyVector[IgnoreBlanks](Asc, Desc)
  lazy val considerBlanks = NonEmptyVector[ConsiderBlanks](AscThenBlanks, DescThenBlanks, BlanksThenAsc, BlanksThenDesc)

  def resolverIB[A](f: (IgnoreBlanks with AscHalf) => A)(reverse: A => A): IgnoreBlanks => A = {
    case Asc  => f(Asc)
    case Desc => reverse(f(Desc.reverse))
  }

  def resolverCB[A](f: (ConsiderBlanks with AscHalf) => A)(reverse: A => A): ConsiderBlanks => A = {
    case c: ConsiderBlanks with AscHalf  => f(c)
    case c: ConsiderBlanks with DescHalf => reverse(f(c.reverse))
  }

  def resolver[A](f: (SortMethod with AscHalf) => A)(reverse: A => A): SortMethod => A = {
    val (a, b) = resolvers(f, f)(reverse)
    merge(a, b)
  }

  def resolvers[A](f: (IgnoreBlanks with AscHalf) => A, g: (ConsiderBlanks with AscHalf) => A)(reverse: A => A) = {
    val ib = resolverIB(f)(reverse)
    val cb = resolverCB(g)(reverse)
    (ib, cb)
  }

  def merge[A](ib: IgnoreBlanks => A, cb: ConsiderBlanks => A): SortMethod => A = {
    case c: IgnoreBlanks   => ib(c)
    case c: ConsiderBlanks => cb(c)
  }

  def nextIB(m: IgnoreBlanks  ): IgnoreBlanks   = Util.nextElement(ignoreBlanks  .whole)(m)
  def nextCB(m: ConsiderBlanks): ConsiderBlanks = Util.nextElement(considerBlanks.whole)(m)
  def next(sm: SortMethod): SortMethod =
    sm match {
      case m: ConsiderBlanks => nextCB(m)
      case m: IgnoreBlanks   => nextIB(m)
    }
}
