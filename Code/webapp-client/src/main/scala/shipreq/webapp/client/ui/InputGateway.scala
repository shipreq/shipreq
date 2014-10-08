package shipreq.webapp.client.ui

import scalaz.Bind

sealed trait InputGateway[M[_], S, I] {
  def i: I
}

final case class EditAllowed[M[_], S, I](i: I, iL: WeirdLens[M, S, S, I]) extends InputGateway[M, S, I]

final case class ReadOnly[M[_], S, I](i: I) extends InputGateway[M, S, I]

object InputGatewayS {

  def map[M[_] : Bind, S, A, B](ig: InputGatewayS[M, S, A], f: A => B, g: (A, B) => A): InputGatewayS[M, S, B] =
    s => implicitly[Bind[M]].map(ig(s)) {
      case EditAllowed(i, il) => EditAllowed(f(i), il.mapF(f)(g))
      case ReadOnly(i)        => ReadOnly(f(i))
    }
}
