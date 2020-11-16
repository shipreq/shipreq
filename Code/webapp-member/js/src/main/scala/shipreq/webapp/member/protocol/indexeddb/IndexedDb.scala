package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react._
import org.scalajs.dom.raw._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.|
import scala.util.{Failure, Success, Try}

final class IndexedDb(raw: IDBFactory) {
  import IndexedDb._
  import IndexedDb.Internals._

  type Result = OpenCallbacks => AsyncCallback[Database]

  def open(name: DatabaseName): Result =
    _open(raw.open(name.value))

  def open(name: DatabaseName, version: Int): Result =
    _open(raw.open(name.value, version))

  private def _open(rawOpen: => IDBOpenDBRequest): Result =
    callbacks => {

      def create(): IDBOpenDBRequest = {
        val r = rawOpen

        r.onblocked = callbacks.blocked.toJsFn1

        r.onupgradeneeded = e => {
          val db = new DatabaseInVersionChange(r.result.asInstanceOf[IDBDatabase])
          val args = versionChange(db, e)
          callbacks.upgradeNeeded(args).runNow()
        }

        r
      }

      asyncRequest(create())(r => new Database(r.result.asInstanceOf[IDBDatabase]))
    }
}

object IndexedDb {

  def apply(raw: IDBFactory): IndexedDb =
    new IndexedDb(raw)

  final case class DatabaseName(value: String)

  final case class Key(value: Int | String) {
    def asJs = value.asInstanceOf[js.Any]
  }

  // ===================================================================================================================

  import Internals._

  final case class VersionChange(db: DatabaseInVersionChange, oldVersion: Int, newVersion: Option[Int])

  final case class OpenCallbacks(upgradeNeeded: VersionChange => Callback,
                                 blocked      : Callback)

  final case class Error(event: ErrorEvent) extends RuntimeException(event.message) {
    import event.{message => msg}

    def isStoredDatabaseHigherThanRequested: Boolean = {
      // Chrome: The requested version (1) is less than the existing version (2).
      // Firefox: The operation failed because the stored database is a higher version than the version requested.
      msg.contains("version") && (msg.contains("higher") || msg.contains("less than"))
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  final class Database(raw: IDBDatabase) {

    def close: Callback =
      Callback {
        raw.close()
      }

    def onVersionChange(f: VersionChange => AsyncCallback[Unit]): Callback =
      Callback {
        raw.asInstanceOf[IDBDatabaseMissing].onversionchange = e => {
          val args = versionChange(new DatabaseInVersionChange(raw), e)
          f(args).runNow()
        }
      }

    val transactionRO: TxnStep1 = new TxnStep1("readonly")
    val transactionRW: TxnStep1 = new TxnStep1("readwrite")

    final class TxnStep1 private[Database] (mode: String) {
      def apply(stores: ObjectStoreDef[_]*): TxnStep2 = {
        val storeArray = new js.Array[String]
        stores.foreach(s => storeArray.push(s.name))
        new TxnStep2(mode, storeArray)
      }
    }

    final class TxnStep2 private[Database] (mode: String, stores: js.Array[String]) {

      def apply[A](f: TxnDsl => Txn[A]): AsyncCallback[A] = {
        val x = CallbackTo.pure(f(TxnDsl))
        sync(_ => x)
      }

      def sync[A](dslCB: TxnDsl => CallbackTo[Txn[A]]): AsyncCallback[A] = {
        for {
          dsl <- dslCB(TxnDsl).asAsyncCallback

          (awaitTxnCompletion, complete) <- AsyncCallback.promise[Unit].asAsyncCallback

          result <- AsyncCallback.byName {
            val txn = raw.transaction(stores, mode)

            txn.onerror = event => {
              complete(Failure(Error(event))).runNow()
            }

            txn.oncomplete = complete(success_).toJsFn1

            Txn.interpret(txn, dsl)
          }

          _ <- awaitTxnCompletion

        } yield result
      }

      def async[A](dsl: TxnDsl => AsyncCallback[Txn[A]]): AsyncCallback[A] =
        dsl(TxnDsl).flatMap(d => apply(_ => d))

    } // TxnStep2

    def add[A](store: ObjectStoreDef.Sync[A])(key: Key, value: A): AsyncCallback[Unit] =
      transactionRW(store)(_.objectStore(store).flatMap(_.add(key, value)))

    def add[A](store: ObjectStoreDef.Async[A])(key: Key, value: A): AsyncCallback[Unit] =
      store.encode(value).flatMap(value =>
        transactionRW(store)(_.objectStore(store).flatMap(_.add(key, value))))

    def get[A](store: ObjectStoreDef.Sync[A])(key: Key): AsyncCallback[Option[A]] =
      transactionRO(store)(_.objectStore(store).flatMap(_.get(key)))

    def get[A](store: ObjectStoreDef.Async[A])(key: Key): AsyncCallback[Option[A]] =
      transactionRO(store)(_.objectStore(store).flatMap(_.get(key)))
        .flatMap(AsyncCallback.traverseOption(_)(_.decode))
  }

  // -------------------------------------------------------------------------------------------------------------------
  final class DatabaseInVersionChange(raw: IDBDatabase) {
    def createObjectStore[A](defn: ObjectStoreDef[A]): Callback =
      Callback {
        raw.createObjectStore(defn.name)
      }
  }

  // -------------------------------------------------------------------------------------------------------------------
  final class ObjectStore[A](val defn: ObjectStoreDef.Sync[A]) {
    import defn.codec

    def add(key: Key, value: A): Txn[Unit] =
      Txn.EvalCallback(codec.encode(value)).flatMap(Txn.StoreAdd(this, key, _))

    def get(key: Key): Txn[Option[A]] =
      Txn.StoreGet(this, key)
  }

  // -------------------------------------------------------------------------------------------------------------------
  final class TxnDsl private[IndexedDb] () {

    // Sync only. Async not allowed by IndexedDB.
    def eval[A](c: CallbackTo[A]): Txn[A] =
      Txn.EvalCallback(c)

    def objectStore[A](s: ObjectStoreDef.Sync[A]): Txn[ObjectStore[A]] =
      Txn.GetStore(s)

    @inline def objectStore[A](s: ObjectStoreDef.Async[A]): Txn[ObjectStore[s.Value]] =
      objectStore(s.sync)
  }

  private val TxnDsl = new TxnDsl()

  // -------------------------------------------------------------------------------------------------------------------

  /** Embedded language for safely working with(in) an IndexedDB transaction.
    *
    * This is necessary because whilst all the transaction methods are async, any other type of asynchronicity is not
    * supported and will result in IndexedDB automatically committing and closing the transaction, in which case,
    * further interaction with the transaction will result in a runtime error.
    *
    * @tparam A The return type.
    */
  sealed trait Txn[A] {
    import Txn._

    final def map[B](f: A => B): Txn[B] =
      Map(this, f)

    final def flatMap[B](f: A => Txn[B]): Txn[B] =
      FlatMap(this, f)
  }

  private object Txn {
    final case class Map         [A, B](from: Txn[A], f: A => B)                        extends Txn[B]
    final case class FlatMap     [A, B](from: Txn[A], f: A => Txn[B])                   extends Txn[B]
    final case class EvalCallback[A]   (callback: CallbackTo[A])                        extends Txn[A]
    final case class GetStore    [A]   (defn: ObjectStoreDef.Sync[A])                   extends Txn[ObjectStore[A]]
    final case class StoreAdd          (store: ObjectStore[_], key: Key, value: js.Any) extends Txn[Unit]
    final case class StoreGet    [A]   (store: ObjectStore[A], key: Key)                extends Txn[Option[A]]

    def interpret[A](txn: IDBTransaction, dsl: Txn[A]): AsyncCallback[A] =
      AsyncCallback.byName {
        val stores = js.Dynamic.literal().asInstanceOf[js.Dictionary[IDBObjectStore]]

        def getStore(s: ObjectStore[_]) =
          AsyncCallback.delay(stores.get(s.defn.name).get)

        def interpret[B](dsl: Txn[B]): AsyncCallback[B] =
          dsl match {

            case FlatMap(fa, f) =>
              interpret(fa).flatMap(a => interpret(f(a)))

            case StoreGet(s, k) =>
              getStore(s).flatMap { store =>
                asyncRequest(store.get(k.asJs))(_.result).flatMapSync { result =>
                  if (js.isUndefined(result))
                    CallbackTo.pure(None)
                  else
                    s.defn.codec.decode(result).map(Some(_))
                }
              }

            case EvalCallback(c) =>
              c.asAsyncCallback

            case GetStore(sd) =>
              AsyncCallback.delay {
                val s = txn.objectStore(sd.name)
                stores.put(sd.name, s)
                new ObjectStore(sd)
              }

            case StoreAdd(s, k, v) =>
              getStore(s).flatMap { store =>
                asyncRequest_ {
                  store.add(v, k.asJs)
                }
              }

            case Map(fa, f) =>
              interpret(fa).map(f)
          }

        interpret(dsl)
      }

  } // Txn

  // ===================================================================================================================

  private object Internals {

    val success_ = Success(())

    def asyncRequest_[R <: IDBRequest](act: => R): AsyncCallback[Unit] =
      asyncRequest(act)(_ => ())

    def asyncRequest[R <: IDBRequest, A](act: => R)(onSuccess: R => A): AsyncCallback[A] =
      AsyncCallback.promise[A].asAsyncCallback.flatMap { case (promise, complete) =>
        val raw = act

        raw.onerror = event => {
          complete(Failure(Error(event))).runNow()
        }

        raw.onsuccess = _ => {
          complete(Try(onSuccess(raw))).runNow()
        }

        promise
      }

    def versionChange(db: DatabaseInVersionChange, e: IDBVersionChangeEvent): VersionChange = {
      // TODO fix after https://github.com/scala-js/scala-js-dom/pull/429
      val newVersion = (e.asInstanceOf[js.Dynamic].newVersion: Any) match {
        case i: Int => Some(i)
        case _      => None
      }
      VersionChange(db, e.oldVersion, newVersion)
    }

    // TODO remove after https://github.com/scala-js/scala-js-dom/pull/433
    @js.native
    @JSGlobal("IDBDatabase")
    @nowarn
    class IDBDatabaseMissing extends IDBDatabase {
      var onversionchange: js.Function1[IDBVersionChangeEvent, _] = js.native
    }
  }

}
