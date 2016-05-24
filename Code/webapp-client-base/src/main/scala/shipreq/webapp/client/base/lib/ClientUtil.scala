package shipreq.webapp.client.base.lib

import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.base.util.NonEmptyVector

object ClientUtil {

  private[this] var GLOBAL_VAR = 0

  val uniqueInt = CallbackTo[Int] {
    // JS is single-threaded
    GLOBAL_VAR += 1
    GLOBAL_VAR
  }

  val uniqueStr: CallbackTo[String] =
    uniqueInt.map(i => s"___uqs$i")

  def textChangeRecv[R](f: String => R): ReactEventI => R =
    e => f(e.target.value)

//  def checkboxOfSetPresence[A](as: Set[A])(a: A, update: Set[A] => Callback): ReactTag = {
//    val currentState = On <~ as.contains(a)
//    def toggled = currentState match {
//      case On  => as - a
//      case Off => as + a
//    }
//    Widgets.checkbox(currentState)(^.onChange --> update(toggled))
//  }

  val sepComma: TagMod = ", "
  val sepSpace: TagMod = " "

  def renderVector[A, B](as: Vector[A], separator: TagMod)(renderEach: A => B)(implicit g: B => TagMod): ReactTag =
    <.span(
      NonEmptyVector.option(as)
        .map(_.intercalateF(separator)(g compose renderEach).whole))

}
