package shipreq.base.db

import cats.~>
import doobie._
import shipreq.base.util.FxModule._

final class XA(private[db] val xa: Transactor[Fx]) {

  @inline def apply[A](c: ConnectionIO[A]): Fx[A] =
    trans(c)

  val trans: ConnectionIO ~> Fx =
    xa.trans
}

object XA {

  @inline implicit def xaAsTransactor(xa: XA): Transactor[Fx] =
    xa.xa
}
