package shipreq.webapp.base.ui.semantic

sealed abstract class ColourPlus(c: ClassName) extends HasClass(c)
object ColourPlus {
  @inline def Default: ColourPlus = Colour.Default
  case object Primary   extends ColourPlus("primary")
  case object Secondary extends ColourPlus("secondary")
  case object Positive  extends ColourPlus("positive")
  case object Negative  extends ColourPlus("negative")
  implicit def univEq: UnivEq[ColourPlus] = UnivEq.derive
}

sealed abstract class Colour(c: ClassName) extends ColourPlus(c)
object Colour {
  case object Default extends Colour(NoClass)
  case object Red     extends Colour("red")
  case object Orange  extends Colour("orange")
  case object Yellow  extends Colour("yellow")
  case object Olive   extends Colour("olive")
  case object Green   extends Colour("green")
  case object Teal    extends Colour("teal")
  case object Blue    extends Colour("blue")
  case object Violet  extends Colour("violet")
  case object Purple  extends Colour("purple")
  case object Pink    extends Colour("pink")
  case object Brown   extends Colour("brown")
  case object Grey    extends Colour("grey")
  case object Black   extends Colour("black")
  implicit def univEq: UnivEq[Colour] = UnivEq.derive
}

sealed abstract class Size(c: ClassName) extends HasClass(c)
object Size {
  case object Mini    extends Size("mini")
  case object Tiny    extends Size("tiny")
  case object Small   extends Size("small")
  case object Default extends Size(NoClass)
  case object Large   extends Size("large")
  case object Big     extends Size("big")
  case object Huge    extends Size("huge")
  case object Massive extends Size("massive")
  implicit def univEq: UnivEq[Size] = UnivEq.derive
}
