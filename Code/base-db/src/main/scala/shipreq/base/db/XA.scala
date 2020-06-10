package shipreq.base.db

import doobie._
import shipreq.base.util.FxModule._

final class XA(private[db] val xa: Transactor[Fx]) {

  @inline def apply[A](c: ConnectionIO[A]): Fx[A] =
    transC(c)

  val transC: cats.~>[ConnectionIO, Fx] =
    xa.trans

  val transZ: scalaz.~>[ConnectionIO, Fx] =
    new scalaz.~>[ConnectionIO, Fx] {
      override def apply[A](fa: ConnectionIO[A]): Fx[A] =
        transC(fa)
    }
}

object XA {

  implicit def xaAsTransactor(xa: XA): Transactor[Fx] =
    xa.xa

}
