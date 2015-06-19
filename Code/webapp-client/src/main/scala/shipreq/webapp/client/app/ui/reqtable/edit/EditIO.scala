package shipreq.webapp.client.app.ui.reqtable.edit

import scalaz.Equal
import scalaz.effect.IO
import shipreq.base.util.SetDiff
import shipreq.webapp.client.app.ui.reqtable.Cell

/**
 * When an editor is ready to be closed, this is the result.
 */
sealed trait EditResult[+A]

case class Save[+A](newValue: A, editCmd: () => Cell.Edit) extends EditResult[A]

case object Abort extends EditResult[Nothing]

// =====================================================================================================================

case class EditIO[A](io: EditResult[A] => IO[Unit]) extends AnyVal {

  def cfmap[B](f: B => Option[A]): EditIO[B] =
    EditIO { br =>
      val ar = br match {
        case Save(b, e) => f(b) match {
          case Some(a) => Save(a, e)
          case None    => Abort
        }
        case Abort => Abort
      }
      io(ar)
    }

  def cmap[B](f: B => A): EditIO[B] =
    cfmap(b => Some(f(b)))

  def ignore(f: A => Boolean): EditIO[A] =
    cfmap(a => if (f(a)) None else Some(a))

  def ignoreIfEqual(initial: A)(implicit e: Equal[A]): EditIO[A] =
    ignore(e.equal(initial, _))

  def cmapToInitial[B: Equal](initial: B)(f: B => A): EditIO[B] =
    cmap(f).ignoreIfEqual(initial)

  def setDiff[B](f: SetDiff[B] => A): EditIO[SetDiff[B]] =
    cmap(f).ignore(_.isEmpty)

  def abortCommit: (IO[Unit], (() => Cell.Edit) => A => IO[Unit]) = {
    val abort = io(Abort)
    val commit = (e: () => Cell.Edit) => (a: A) => io(Save(a, e))
    (abort, commit)
  }
}

