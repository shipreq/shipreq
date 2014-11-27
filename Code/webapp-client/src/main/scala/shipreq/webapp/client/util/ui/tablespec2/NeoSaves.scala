package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.base.validation2._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.util.ui.table.SuccessIO
import scalaz.{Need, Name}
import scalaz.effect.IO

object NeoSaves {

  type SetRowStatus[S] = RowStatus => ReactS[S, Unit]

  type Retry[S] = Name[ReactST[IO, S, Unit]]

  sealed trait SaveNeed {
    def asOption[A](a: A): Option[A]
  }
  case object SaveNeeded extends SaveNeed {
    override def asOption[A](a: A) = Some(a)
  }
  case object SaveNotNeeded extends SaveNeed {
    override def asOption[A](a: A) = None
  }


//  def validateS[S, I, O](v: Validator[S, I, _, O], i: S => I): ReactS[S, ValidationResult[O]] =
//     ReactS.gets(s => v.correctAndValidate(s, i(s)))

  def retryably[A](f: Name[A] => A): A = {
    lazy val a: A = f(Need(a))
    a
  }

//  def validateAndSaveAsync2[K, S, T, P, U, I](validator: Validator[T, I, _, U],
//                                              st: S => T,
//                                              srs: SavedRowStore[S, K, P, I],
//                                              k: K,
//                                              needSave: (U, P) => SaveNeed,
//                                              asyncSaveIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
//                                              realise: ReactST[IO, S, Unit] => IO[Unit])
//  : ReactST[IO, S, Unit] =
//    validateAndSaveAsync(validator, st, srs getI k, srs getP k, needSave, asyncSaveIO, realise, srs setStatusS k)

  def validateAndSaveAsync[S, T, P, U, I](validator: Validator[T, I, _, U],
                                          st: S => T,
                                          si: S => I,
                                          sp: S => P,
                                          needSave: (U, P) => SaveNeed,
                                          asyncSaveIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
                                          realise: ReactST[IO, S, Unit] => IO[Unit],
                                          setStatus: SetRowStatus[S])
  : ReactST[IO, S, Unit] = {
    type R = ReactST[IO, S, Unit]
    val Fix = ReactS.FixT[IO, S] // TODO should add type X[A] = ReactST[M, S, A] to FixT
    retryably[R](retry => {
      def abortSave: R = setStatus(RowStatus.Sync).lift[IO]
      def valid(u: U): R = Fix.liftR { s =>
        val p = sp(s)
        needSave(u, p) match {
          case SaveNotNeeded => abortSave
          case SaveNeeded    => save(p, u) >> setStatus(RowStatus.Locked).lift[IO]
        }
      }
      def save(p: P, u: U): R = {
        val s: SuccessIO = SuccessIO.nop
        val f: FailureIO = {
          val retryIO  = realise(retry.value)
          val failureS = setStatus(RowStatus.Failed(retryIO)).lift[IO]
          FailureIO(realise(failureS))
        }
        Fix.retM(asyncSaveIO(p, u, s, f))
      }
      val x = Fix.gets(s => IO(validator.correctAndValidate(st(s), si(s))))
      x.flatMap(_.fold(_ => abortSave, valid))
    })
  }
}
