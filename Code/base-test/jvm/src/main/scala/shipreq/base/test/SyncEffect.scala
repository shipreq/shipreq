package shipreq.base.test

trait SyncEffect[F[_]] {
  def unsafeRun[A](fa: F[A]): A
}

object SyncEffect {

  import shipreq.base.util.FxModule._
  implicit val syncEffectFx: SyncEffect[Fx] =
    new SyncEffect[Fx] {
      override def unsafeRun[A](fa: Fx[A]): A = new FxOps(fa).unsafeRun()
    }

  import scalaz.Name
  implicit val syncEffectName: SyncEffect[Name] =
    new SyncEffect[Name] {
      override def unsafeRun[A](fa: Name[A]): A = fa.value
    }

  object Ops {
    implicit final class SyncEffectOps[F[_], A](private val self: F[A]) extends AnyVal {
      def unsafeRun()(implicit F: SyncEffect[F]): A =
        F.unsafeRun(self)
    }
  }
}