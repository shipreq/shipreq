package shipreq.base.test

import cats.Eval
import shipreq.base.util.FxModule._

trait SyncEffect[F[_]] {
  def unsafeRun[A](fa: F[A]): A
}

object SyncEffect {

  implicit val syncEffectFx: SyncEffect[Fx] =
    new SyncEffect[Fx] {
      override def unsafeRun[A](fa: Fx[A]): A = new FxOps(fa).unsafeRun()
    }

  implicit val syncEffectName: SyncEffect[Eval] =
    new SyncEffect[Eval] {
      override def unsafeRun[A](fa: Eval[A]): A = fa.value
    }

  object Ops {
    implicit final class SyncEffectOps[F[_], A](private val self: F[A]) extends AnyVal {
      def unsafeRun()(implicit F: SyncEffect[F]): A =
        F.unsafeRun(self)
    }
  }
}