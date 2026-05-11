package shipreq.webapp.server.logic.test

import cats.Monad
import cats.syntax.all._
import java.time.{Duration, Instant}
import shipreq.base.test.SyncEffect
import shipreq.base.util.CatsExtra.ApplicativeDelay
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.server.logic.algebra._

final class MockServer[F[_]]()(implicit F: Monad[F], se: SyncEffect[F]) extends Server.Algebra[F] {
  @volatile var clock = Instant.now()

  override val now =
    F.delay(clock)

  def incTime(d: Duration): Unit =
    clock = clock.plus(d)

  def incTimeMs(ms: Long): Unit =
    clock = clock.plusMillis(ms)

  def incTimeSec(sec: Long): Unit =
    clock = clock.plusSeconds(sec)

  override def measureDuration[A](f: F[A]): F[(A, Duration)] =
    for {
      start <- now
      a     <- f
      end   <- now
    } yield (a, Duration.between(start, end))

  override def measureDuration_[A](f: F[A]) =
    measureDuration(f).map(_._2)

  private def durationBorder(duration: Duration, tolerance: Duration = Duration.ofSeconds(2)): Validity => Duration = {
    case Valid   => duration minus tolerance
    case Invalid => duration plus tolerance
  }

  def forwardTimeToEndOfWindow(w: Duration, v: Validity): Unit =
    clock = clock plus durationBorder(w)(v)

  var onDelay = List.empty[() => Unit]
  override def delay[A](f: F[A], d: Duration) = F.delay[A] {
    clock = clock plus d
    onDelay match {
      case Nil    => ()
      case h :: t => onDelay = t; h()
    }
    se.unsafeRun(f)
  }

  var forked = Vector.empty[F[_]]
  override def fork[A](f: F[A]) = F.delay[Unit] {
    forked :+= f
  }
  def runForked(): Unit = {
    forked.foreach(se.unsafeRun(_))
    forked = Vector.empty
  }

  var nextClientIP = Option.empty[IP]
  override val clientIP = F.delay(nextClientIP)
}
