package shipreq.webapp.client.ui

import scalaz.Bind
import scalaz.syntax.bind._
import shipreq.webapp.client.ui.Implicits.Optional2
import Implicits._

final class InputGateway[M[_] : Bind : Optional2, S, R, A, T](
    getT: S => M[T], tr: T => R, ta: T => A, val setA: (S, A) => Option[S]) {

  val getRA: S => M[(R, A)] =
    getT(_).map(t => tr(t) -> ta(t))

  val getA: S => M[A] =
    getT(_) map ta

  def map[B](f: A => B)(g: (A, B) => A) = new InputGateway[M, S, R, B, T](
    getT, tr, f compose ta, (s, b) => getA(s).toOption.flatMap(a => setA(s, g(a, b))))
}
