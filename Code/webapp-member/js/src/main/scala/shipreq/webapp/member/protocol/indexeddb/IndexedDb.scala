package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react._
import org.scalajs.dom.raw._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.|
import scala.util.{Failure, Success, Try}

final class IndexedDb(raw: IDBFactory) {
  import IndexedDb._
  import IndexedDb.Dsl._
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

  // ===================================================================================================================

  import Dsl._

  final case class DatabaseName(value: String) extends AnyVal

  final case class ObjectStoreDef[A](name: String)
                                    (implicit val codec: IndexedDbCodec[A])

  final case class Key(value: Int | String) {
    def js = value.asInstanceOf[scala.scalajs.js.Any]
  }

  final case class VersionChange(db: DatabaseInVersionChange, oldVersion: Int, newVersion: Option[Int])

  final case class OpenCallbacks(upgradeNeeded: VersionChange => AsyncCallback[Unit],
                                 blocked      : Callback)

//  sealed abstract class Error extends RuntimeException
//
//  object Error {
//
//    case object DatabaseHasLaterVersion extends Error
//
//    final case class Generic(event: ErrorEvent) extends Error
//
//    def apply(event: ErrorEvent): Error = {
//      val msg = event.message
//
//      // Chrome: The requested version (1) is less than the existing version (2).
//      // Firefox: The operation failed because the stored database is a higher version than the version requested.
//      @inline def laterVersion =
//        msg.contains("version") && (msg.contains("higher") || msg.contains("less than"))
//
//      if (laterVersion)
//        DatabaseHasLaterVersion
//      else
//        Generic(event)
//    }
//  }

  final case class Error(event: ErrorEvent) extends RuntimeException(event.message) {
    import event.{message => msg}

    def isStoredDatabaseHigherThanRequested: Boolean = {
      // Chrome: The requested version (1) is less than the existing version (2).
      // Firefox: The operation failed because the stored database is a higher version than the version requested.
      msg.contains("version") && (msg.contains("higher") || msg.contains("less than"))
    }
  }

  // ===================================================================================================================

  object Dsl {
    import Internals._

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

      def transactionRO(stores: ObjectStoreDef[_]*): CallbackTo[Transaction] =
        transaction(stores, "readonly")

      def transactionRW(stores: ObjectStoreDef[_]*): CallbackTo[Transaction] =
        transaction(stores, "readwrite")

      private def transaction(stores: Seq[ObjectStoreDef[_]], mode: String): CallbackTo[Transaction] = {
        val a = new js.Array[String]
        stores.foreach(s => a.push(s.name))
        newTransaction(raw.transaction(a, mode))
      }
    }

    // -----------------------------------------------------------------------------------------------------------------
    final class DatabaseInVersionChange(raw: IDBDatabase) {
      def createObjectStore[A](defn: ObjectStoreDef[A]): CallbackTo[ObjectStore[A]] =
        CallbackTo {
          val os = raw.createObjectStore(defn.name)
          new ObjectStore(os, defn)
        }
    }

    // -----------------------------------------------------------------------------------------------------------------
    def newTransaction(raw: IDBTransaction): CallbackTo[Transaction] =
      AsyncCallback.promise[Unit].map { case (promise, complete) =>

        raw.onerror = event => {
          complete(Failure(Error(event))).runNow()
        }

        raw.oncomplete = complete(Success(())).toJsFn1

        new Transaction(raw, promise)
      }

    final class Transaction(raw: IDBTransaction, val completion: AsyncCallback[Unit]) {

      def objectStore[A](defn: ObjectStoreDef[A]): CallbackTo[ObjectStore[A]] =
        CallbackTo {
          val os = raw.objectStore(defn.name)
          new ObjectStore(os, defn)
        }
    }

    // -----------------------------------------------------------------------------------------------------------------
    final class ObjectStore[A](raw: IDBObjectStore, defn: ObjectStoreDef[A]) {
      import defn.codec

      def add(key: Key, value: A): AsyncCallback[Unit] =
        asyncRequest_(raw.add(codec.encode(value), key.js))

      def get(key: Key): AsyncCallback[Option[A]] =
        asyncRequest(raw.get(key.js)) { r =>
          val x = r.result
          Option.unless(js.isUndefined(x))(codec.decodeOrThrow(x))
        }
    }
  }

  // ===================================================================================================================

  private object Internals {

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
      val newVersion = (e.asInstanceOf[js.Dynamic].newVersion: Any) match {
        case i: Int => Some(i)
        case _      => None
      }
      VersionChange(db, e.oldVersion, newVersion)
    }

    @js.native
    @JSGlobal("IDBDatabase")
    @nowarn
    class IDBDatabaseMissing extends IDBDatabase {
      var onversionchange: js.Function1[IDBVersionChangeEvent, _] = js.native
    }
  }

}
