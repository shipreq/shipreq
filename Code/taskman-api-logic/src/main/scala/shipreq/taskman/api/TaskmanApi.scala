package shipreq.taskman.api

import scalaz.{Applicative, Monad, Traverse, ~>}
import scalaz.syntax.monad.ToBindOps
import scalaz.syntax.traverse._

trait TaskmanApi[F[_]] {

  /**
   * Stores/updates a configuration value that will be read by the Taskman server.
   *
   * @param key The config key.
   * @param value The config value.
   */
  def cfgPut(key: String, value: String): F[Unit]

  /** Submits a Msg to the Taskman server for processing. */
  def submitMsg(m: Msg): F[MsgId]

  /** Inspects the status of a msg. */
  def queryMsgStatus(id: MsgId): F[Option[MsgStatus]]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final def cfgPutBulk(kvs: (String, String)*)(implicit F: Monad[F]): F[Unit] =
    if (kvs.isEmpty)
      F.pure(())
    else
      kvs.iterator.map(kv => cfgPut(kv._1, kv._2)).reduce(_ >> _)

  /** Submits 0-n Msgs to the Taskman server for processing. */
  final def submitMsgs[G[_] : Traverse](ms: G[Msg])(implicit F: Applicative[F]): F[G[(Msg, MsgId)]] =
    ms.traverse(m => submitMsg(m).map((m, _)))

  /** Submits 0-n Msgs to the Taskman server for processing, discarding the results. */
  final def submitMsgs_[G[_] : Traverse](ms: G[Msg])(implicit F: Applicative[F]): F[Unit] =
    ms.traverse_(submitMsg(_).void)
}

object TaskmanApi {
  def trans[F[_], G[_]](f: TaskmanApi[F])(g: F ~> G): TaskmanApi[G] =
    new TaskmanApi[G] {
      override def cfgPut(k: String, v: String) = g(f.cfgPut(k, v))
      override def submitMsg(m: Msg)            = g(f.submitMsg(m))
      override def queryMsgStatus(id: MsgId)    = g(f.queryMsgStatus(id))
    }
}