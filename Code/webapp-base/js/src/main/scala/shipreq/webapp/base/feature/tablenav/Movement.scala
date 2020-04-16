package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.nonempty.NonEmptyVector
import scala.annotation.tailrec
import scalaz.{-\/, Applicative, \/, \/-}
import scalaz.Scalaz.Id
import shipreq.base.util.{Allow, Permission, Util}

sealed trait Axis
object Axis {
  case object UpDown extends Axis
  case object LeftRight extends Axis
}

sealed abstract class Movement(val adjustIndex: Int \/ (Int => Int), val reAdjustIndex: Int => Int) {

  private def adjustedIndexF[F[_], A](givenIndex: => F[Int])(implicit F: Applicative[F]): F[Int] =
    adjustIndex match {
      case \/-(f) => F.map(givenIndex)(f)
      case -\/(i) => F.pure(i)
    }

  private def lookup[A](as: IndexedSeq[A], i: Int): Option[A] =
    Option.when(as.nonEmpty)(
      as(Util.fitCollectionIndex(i, as.length)))

  final def moveF[F[_], A](as: IndexedSeq[A], currentIndex: => F[Int])(implicit F: Applicative[F]): F[Option[A]] =
    F.map(adjustedIndexF(currentIndex))(lookup(as, _))

  final def move[A](as: IndexedSeq[A], currentIndex: => Int): Option[A] =
    moveF[Id, A](as, currentIndex)

  private def lookupNev[A](as: NonEmptyVector[A], i: Int): A =
    as.unsafeApply(Util.fitCollectionIndex(i, as.length))

  final def moveNevF[F[_], A](as: NonEmptyVector[A], currentIndex: => F[Int])(implicit F: Applicative[F]): F[A] =
    F.map(adjustedIndexF(currentIndex))(lookupNev(as, _))

  final def moveNev[A](as: NonEmptyVector[A], currentIndex: => Int): A =
    moveNevF[Id, A](as, currentIndex)

  final def moveSelectiveF[F[_], A](as: IndexedSeq[A], currentIndex: => F[Int])
                                   (allow: (A, Int) => Permission)
                                   (implicit F: Applicative[F]): F[Option[A]] = {
    @tailrec
    def attempt(attempts: Int, nextIndex: Int): Option[A] = {
      val i = Util.fitCollectionIndex(nextIndex, as.length)
      val a = as(i)
      if (allow(a, i) is Allow)
        Some(a)
      else if (attempts < 0)
        None
      else
        attempt(attempts - 1, reAdjustIndex(nextIndex))
    }

    if (as.isEmpty)
      F.pure(None)
    else
      F.map(adjustedIndexF(currentIndex))(attempt(as.length, _))
  }

  final def moveSelective[A](as: IndexedSeq[A], currentIndex: => Int)
                            (allow: (A, Int) => Permission): Option[A] =
    moveSelectiveF[Id, A](as, currentIndex)(allow)
}

object Movement {
  case object Prev extends Movement(\/-(_ - 1), _ - 1)
  case object Next extends Movement(\/-(_ + 1), _ + 1)
  case object Head extends Movement(-\/(0), _ + 1)
  case object Last extends Movement(-\/(-1), _ - 1)
}
