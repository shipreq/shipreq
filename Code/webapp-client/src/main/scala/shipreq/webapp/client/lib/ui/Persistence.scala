package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ScalazReact._
import scalaz.{Need, Name}
import scalaz.effect.IO
import shipreq.webapp.base.event.{VerifiedEvents, DeletionAction}
import shipreq.webapp.base.protocol.RemoteFn
import shipreq.webapp.base.validation._
import shipreq.webapp.client.lib.{CrudIO, FailureIO, SuccessIO}
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.ClientData

object Persistence {

  type ST[S]           = ReactST[IO, S, Unit]
  type SetRowStatus[S] = RowStatus => ST[S]
  type Retry[S]        = Name[ST[S]]
  type Realise[S]      = ST[S] => IO[Unit]

  def retryably[A](f: Name[A] => A): A = {
    lazy val a: A = f(Need(a))
    a
  }

  def failureIO[S](retry: Retry[S])(realise: Realise[S], setStatus: SetRowStatus[S]): FailureIO = {
    def failedStatus = RowStatus.Failed.lazily(realise(retry.value))
    FailureIO(realise(setStatus(failedStatus)))
  }


  // ===================================================================================================================
  // Update

  def asyncUpdate[S, T, P, U, I](validator: Validator[T, I, _, U],
                                 st: S => T,
                                 si: S => I,
                                 sp: S => P,
                                 setStatus: SetRowStatus[S],
                                 needSave: (P, U) => SaveNeed,
                                 updateIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
                                 realise: Realise[S]): ST[S] = {
    val Fix = ReactS.FixT[IO, S]
    type R = Fix.T[Unit]

    retryably[R](retry => {
      def abortSave: R = setStatus(RowStatus.Sync)
      def valid(u: U): R = Fix.liftR { s =>
        val p = sp(s)
        needSave(p, u) match {
          case SaveNotNeeded => abortSave
          case SaveNeeded    => save(p, u) >> setStatus(RowStatus.Locked)
        }
      }
      def save(p: P, u: U): R = {
        val s: SuccessIO = SuccessIO.nop
        val f = failureIO(retry)(realise, setStatus)
        Fix.ret(updateIO(p, u, s, f))
      }
      Fix.liftR(s =>
        validator.correctAndValidate(st(s), si(s))
          .fold(_ => abortSave, valid))
    })
  }


  def asyncUpdateS[S, T, K, P, U, I](validator: Validator[T, I, _, U], store: SavedRowStore[S, K, P, I])
                                    (st: K => S => T,
                                     needSave: (P, U) => SaveNeed,
                                     updateIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
                                     realise: Realise[S]): K => ST[S] =
    k => asyncUpdate(
      validator, st(k),
      store.getI(k),
      store.getP(k),
      store.setStatusST[IO](k),
      needSave, updateIO, realise)


  def simpleAsyncUpdate[S, K, P, I, R <: RemoteFn.AuxG[(K, I), VerifiedEvents]](store   : SavedRowStore[S, K, P, I])
                                                                               (remoteFn: RemoteFn.InstanceFor[R],
                                                                                cd      : ClientData,
                                                                                cp      : ClientProtocol,
                                                                                realise : Persistence.Realise[S],
                                                                                id      : K): ST[S] =
    ReactS.liftR[IO, S, Unit](state => {
      val setStatus = store.setStatusST[IO](id)
      val saveio = retryably[ReactST[IO, S, Unit]](retry => {
        val v = store.getI(id)(state)
        val f = Persistence.failureIO(retry)(realise, setStatus)
        val io = cp.call(remoteFn)((id, v), cd.applyEventsS, cp.consumeGenericFailure(_) >> f.io)
        ReactS retM io
      })
      saveio >> setStatus(RowStatus.Locked)
    })

  // ===================================================================================================================
  // Create

  def asyncCreate[S, T, U, I](validator: Validator[T, I, _, U],
                              st: S => T,
                              si: S => Option[I],
                              removeNew: ReactS[S, Unit],
                              setStatus: SetRowStatus[S],
                              createIO: (U, SuccessIO, FailureIO) => IO[Unit],
                              realise: Realise[S]): ST[S] = {
    val Fix = ReactS.FixT[IO, S]
    type R = Fix.T[Unit]

    retryably[R](retry => {
      def abortSave: R = setStatus(RowStatus.Sync)
      def valid(u: U): R = Fix.liftR { s =>
        save(u) >> setStatus(RowStatus.Locked)
      }
      def save(u: U): R = {
        val s = SuccessIO(realise(removeNew.liftIO))
        val f = failureIO(retry)(realise, setStatus)
        Fix.ret(createIO(u, s, f))
      }
      Fix.liftR(s =>
        si(s).fold(Fix.nop)(i =>
          validator.correctAndValidate(st(s), i)
            .fold(_ => abortSave, valid)))
    })
  }


  def asyncCreateS[S, T, U, I](validator: Validator[T, I, _, U], store: NewRowStore[S, I])
                              (st: S => T,
                               createIO: (U, SuccessIO, FailureIO) => IO[Unit],
                               realise: Realise[S]): ST[S] =
    asyncCreate(
      validator, st,
      store.getI,
      ReactS mod store.remove,
      store.setStatusST[IO],
      createIO, realise)


  // ===================================================================================================================
  // Save = Create | Update

  def asyncSaveS[S, T, K, P, U, I](v: Validator[T, I, _, U], savedStore: SavedRowStore[S, K, P, I])
                                  (newStore: NewRowStore[S, I],
                                   createT: S => T,
                                   updateT: K => S => T,
                                   needSave: (P, U) => SaveNeed,
                                   createIO: (U, SuccessIO, FailureIO) => IO[Unit],
                                   updateIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
                                   realise: Realise[S]): Option[K] => ST[S] = {
    val create = asyncCreateS(v, newStore)(createT, createIO, realise)
    val update = asyncUpdateS(v, savedStore)(updateT, needSave, updateIO, realise)
    _.fold(create)(update)
  }


  def asyncSaveT[K, P, U, I](v: Validator[(Stream[P], Option[K]), I, _, U], sas: TypicalStoresAndState[P, I, K])
                            (needSave: (P, U) => SaveNeed,
                             crudIO: CrudIO[P, _, U, _],
                             realise: sas.ST => IO[Unit]): Option[K] => sas.ST =
    asyncSaveS(
      v, sas.savedRowStoreS)(sas.newRowStoreS,
        sas.validatorInput(None), k => sas.validatorInput(Some(k)), needSave,
        crudIO.createIO, crudIO.updateIO, realise)


  def asyncSaveNS[S, T, K, P, U, I](v: Validator[T, I, _, U],
                                    stores: NewAndSavedStores[S, K, P, I],
                                    createIO: (U, SuccessIO, FailureIO) => IO[Unit])
                                   (updateIO: (P, U, SuccessIO, FailureIO) => IO[Unit],
                                    needSave: (P, U) => SaveNeed,
                                    t: Option[K] => S => T,
                                    realise: Realise[S]): Option[K] => ST[S] =
    asyncSaveS(v, stores.s)(stores.n, t(None), k => t(Some(k)), needSave, createIO, updateIO, realise)


  def asyncSaveNS2[S, T, K, P, U, I](v: Validator[T, I, _, U],
                                     stores: NewAndSavedStores[S, K, P, I],
                                     createIO: U => (SuccessIO, FailureIO) => IO[Unit])
                                    (updateIO: (K, U) => (SuccessIO, FailureIO) => IO[Unit],
                                     needSave: (P, U) => SaveNeed,
                                     pk: P => K,
                                     t: Option[K] => S => T,
                                     realise: Realise[S]): Option[K] => ST[S] =
    asyncSaveS(v, stores.s)(stores.n, t(None), k => t(Some(k)), needSave,
      (u, s, f)    => createIO(       u)(s, f),
      (p, u, s, f) => updateIO(pk(p), u)(s, f),
      realise)



  // ===================================================================================================================
  // Delete

  def asyncDelete[S](deleteIO: (SuccessIO, FailureIO) => IO[Unit],
                     realise: Realise[S],
                     setStatus: SetRowStatus[S]): ST[S] = {
    val Fix = ReactS.FixT[IO, S]
    type R = Fix.T[Unit]

    retryably[R](retry => {
      val s = SuccessIO.nop
      val f = failureIO(retry)(realise, setStatus)
      Fix.ret(deleteIO(s, f)) >> setStatus(RowStatus.Locked)
    })
  }


  def asyncDeletionS[S, K](store: SavedRowStore[S, K, _, _])
                          (deleteIO: (K, DeletionAction) => (SuccessIO, FailureIO) => IO[Unit],
                           realise: Realise[S]): Deletion[K] =
    new Deletion[K]((id, a) =>
      realise(asyncDelete(deleteIO(id, a), realise, store.setStatusST[IO](id))))
}