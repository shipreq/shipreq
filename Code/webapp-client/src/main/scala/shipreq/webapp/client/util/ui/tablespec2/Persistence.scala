package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.Alive
import shipreq.webapp.base.protocol.DeletionAction
import shipreq.webapp.base.validation2._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.util.ui.table.SuccessIO
import scalaz.{Equal, Need, Name}
import scalaz.effect.IO
import scalaz.syntax.equal._

object Persistence {

  type SetRowStatus[S] = RowStatus => ReactST[IO, S, Unit]

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

  object SaveNeed {
    def cmpToExtract[A, B: Equal](f: A => B): (A, B) => SaveNeed = {
      val c = cmp[B]
      (a, b) => c(f(a), b)
    }

    def cmp[A: Equal]: (A, A) => SaveNeed =
      (a, b) => if (a ≟ b) SaveNotNeeded else SaveNeeded
  }


  def retryably[A](f: Name[A] => A): A = {
    lazy val a: A = f(Need(a))
    a
  }

  def validateAndSaveAsync2[S, T, K, P, U, I](validator: Validator[T, I, _, U], store: SavedRowStore[S, K, P, I])(
    st: K => S => T,
    needSave: (P, U) => SaveNeed,
    asyncSaveIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
    realise: ReactST[IO, S, Unit] => IO[Unit])
  : K => ReactST[IO, S, Unit] =
    k => validateAndSaveAsync(
      validator, st(k),
      store.getI(k),
      store.getP(k),
      store.setStatusST[IO](k),
      needSave, asyncSaveIO, realise)

  def validateAndSaveAsync[S, T, P, U, I](validator: Validator[T, I, _, U],
                                          st: S => T,
                                          si: S => I,
                                          sp: S => P,
                                          setStatus: SetRowStatus[S],
                                          needSave: (P, U) => SaveNeed,
                                          asyncSaveIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
                                          realise: ReactST[IO, S, Unit] => IO[Unit])
  : ReactST[IO, S, Unit] = {
    val Fix = ReactS.FixT[IO, S]
    type R = Fix.T[Unit]
    retryably[R](retry => {
      def abortSave: R = setStatus(RowStatus.Sync)
      def valid(u: U): R = Fix.liftR { s =>
        val p = sp(s)
        needSave(p, u) match {
          case SaveNotNeeded => abortSave
          case SaveNeeded => save(p, u) >> setStatus(RowStatus.Locked)
        }
      }
      def save(p: P, u: U): R = {
        val s: SuccessIO = SuccessIO.nop
        val f = failureIO(retry, realise, setStatus)
        Fix.ret(asyncSaveIO(p, u, s, f))
      }
      Fix.liftR(s =>
        validator.correctAndValidate(st(s), si(s))
          .fold(_ => abortSave, valid))
    })
  }

  def validateAndCreateAsync2[S, T, U, I](validator: Validator[T, I, _, U], store: NewRowStore[S, I])(
    st: S => T,
    asyncCreate: (U, SuccessIO, FailureIO) => IO[Unit],
    realise: ReactST[IO, S, Unit] => IO[Unit]) =
    validateAndCreateAsync[S, T, U, I](
      validator, st, store.getI, ReactS mod store.remove, store.setStatusST[IO], asyncCreate, realise)


  def validateAndCreateAsync[S, T, U, I](validator: Validator[T, I, _, U],
                                         st: S => T,
                                         si: S => Option[I],
                                         removeNew: ReactS[S, Unit],
                                         setStatus: SetRowStatus[S],
                                         asyncCreate: (U, SuccessIO, FailureIO) => IO[Unit],
                                         realise: ReactST[IO, S, Unit] => IO[Unit])
  : ReactST[IO, S, Unit] = {
    val Fix = ReactS.FixT[IO, S]
    type R = Fix.T[Unit]
    retryably[R](retry => {
      def abortSave: R = setStatus(RowStatus.Sync)
      def valid(u: U): R = Fix.liftR { s =>
        save(u) >> setStatus(RowStatus.Locked)
      }
      def save(u: U): R = {
        val s = SuccessIO(realise(removeNew.liftIO))
        val f = failureIO(retry, realise, setStatus)
        Fix.ret(asyncCreate(u, s, f))
      }
      Fix.liftR(s =>
        si(s).fold(Fix.nop)(i =>
          validator.correctAndValidate(st(s), i)
            .fold(_ => abortSave, valid)))
    })
  }

  def deleterAsync[S, K, P](store: SavedRowStore[S, K, P, _])
                           (alive: P => Alive,
                            asyncDelete: (K, DeletionAction) => (SuccessIO, FailureIO) => IO[Unit],
                            realise: ReactST[IO, S, Unit] => IO[Unit]) =
    new Deletion[P, K](alive, (id, a) =>
      realise(deleteAsync(asyncDelete(id, a), realise, store.setStatusST[IO](id))))


  def deleteAsync[S](asyncDelete: (SuccessIO, FailureIO) => IO[Unit],
                     realise: ReactST[IO, S, Unit] => IO[Unit],
                     setStatus: SetRowStatus[S])
  : ReactST[IO, S, Unit] = {
    val Fix = ReactS.FixT[IO, S]
    type R = Fix.T[Unit]
    retryably[R](retry => {
      val s = SuccessIO.nop
      val f = failureIO(retry, realise, setStatus)
      Fix.ret(asyncDelete(s, f)) >> setStatus(RowStatus.Locked)
    })
  }


  def failureIO[S](retry: Retry[S],
                   realise: ReactST[IO, S, Unit] => IO[Unit],
                   setStatus: SetRowStatus[S]): FailureIO = {
    def failedStatus = RowStatus.Failed.lazily(realise(retry.value))
    FailureIO(realise(setStatus(failedStatus)))
  }

  def validateAndSaveBoth[S, T, K, P, U, I](v: Validator[T, I, _, U], savedStore: SavedRowStore[S, K, P, I])(
    newStore: NewRowStore[S, I],
    createT: S => T,
    updateT: K => S => T,
    needSave: (P, U) => SaveNeed,
    asyncCreate: (U, SuccessIO, FailureIO) => IO[Unit],
    asyncSaveIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
    realise: ReactST[IO, S, Unit] => IO[Unit]): Option[K] => ReactST[IO, S, Unit] = {

    val update = validateAndSaveAsync2(v, savedStore)(updateT, needSave, asyncSaveIO, realise)
    val create = validateAndCreateAsync2(v, newStore)(createT, asyncCreate, realise)
    _.fold(create)(update)
  }

  def typicalValidateAndSave[K, P, U, I](v: Validator[(Stream[P], Option[K]), I, _, U], sas: TypicalStoresAndState[P, I, K])(
    needSave: (P, U) => SaveNeed,
    tableIO: TableIO[P, _, U, _],
    realise: sas.ST => IO[Unit]): Option[K] => sas.ST =
    validateAndSaveBoth(
      v, sas.savedRowStoreS)(sas.newRowStoreS,
        sas.validatorInput(None), k => sas.validatorInput(Some(k)), needSave,
        tableIO.createIO, tableIO.updateIO, realise)

}