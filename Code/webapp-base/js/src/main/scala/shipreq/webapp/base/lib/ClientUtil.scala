package shipreq.webapp.base.lib

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.NonEmptyArraySeq

object ClientUtil {

  private[this] var GLOBAL_VAR = 0

  val uniqueInt = CallbackTo[Int] {
    // JS is single-threaded
    GLOBAL_VAR += 1
    GLOBAL_VAR
  }

  val uniqueStr: CallbackTo[String] =
    uniqueInt.map(i => s"___uqs$i")

  def textChangeRecv[R](f: String => R): ReactEventFromInput => R =
    e => f(e.target.value)

//  def checkboxOfSetPresence[A](as: Set[A])(a: A, update: Set[A] => Callback): VdomTag = {
//    val currentState = On <~ as.contains(a)
//    def toggled = currentState match {
//      case On  => as - a
//      case Off => as + a
//    }
//    Widgets.checkbox(currentState)(^.onChange --> update(toggled))
//  }

  val sepComma: TagMod = ", "
  val sepSpace: TagMod = " "

  def renderVector[A, B](as: Vector[A], separator: TagMod)(renderEach: A => B)(implicit g: B => TagMod): TagMod =
    NonEmptyVector.option(as).whenDefined(as =>
      TagMod.fromTraversableOnce(
        as.intercalateF(separator)(g compose renderEach).whole))

  def renderArraySeq[A, B](as: ArraySeq[A], separator: TagMod)(renderEach: A => B)(implicit g: B => TagMod): TagMod =
    NonEmptyArraySeq.option(as).whenDefined(as =>
      TagMod.fromTraversableOnce(
        as.intercalateF(separator)(g compose renderEach).whole))

  def renderSeq[A](as: IterableOnce[A], separator: TagMod)(implicit f: A => TagMod): TagMod =
    TagMod.fromTraversableOnce(
      as.iterator
        .map(f)
        .intersperse(separator))

}
