package shipreq.taskman.api

import scalaz.syntax.monad.ToBindOps
import scalaz.syntax.traverse._
import scalaz.{Applicative, Monad, Traverse, ~>}
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger

trait TaskmanApi[F[_]] { self =>

  /**
   * Stores/updates a configuration value that will be read by the Taskman server.
   *
   * @param key The config key.
   * @param value The config value.
   */
  def cfgPut(key: String, value: String): F[Unit]

  /** Submits a [[Task]] to the Taskman server for processing. */
  def submit(m: Task): F[TaskId]

  /** Inspects the status of a task. */
  def getStatus(id: TaskId): F[Option[TaskStatus]]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def cfgPutBulk(kvs: (String, String)*)(implicit F: Monad[F]): F[Unit] =
    if (kvs.isEmpty)
      F.pure(())
    else
      kvs.iterator.map(kv => cfgPut(kv._1, kv._2)).reduce(_ >> _)

  /** Submits 0-n tasks to the Taskman server for processing. */
  def submitBulk[G[_] : Traverse](ms: G[Task])(implicit F: Applicative[F]): F[G[(Task, TaskId)]] =
    ms.traverse(m => submit(m).map((m, _)))

  /** Submits 0-n tasks to the Taskman server for processing, discarding the results. */
  def submitBulk_[G[_] : Traverse](ms: G[Task])(implicit F: Applicative[F]): F[Unit] =
    ms.traverse_(submit(_).void)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final def trans[G[_]](g: F ~> G)(implicit F: Monad[F]): TaskmanApi[G] =
    new TaskmanApi[G] {
      override def cfgPut(k: String, v: String) = g(self.cfgPut(k, v))
      override def submit(m: Task)              = g(self.submit(m))
      override def getStatus(id: TaskId)        = g(self.getStatus(id))

      // In practice, these override merge actions into a single DB transaction
      override def cfgPutBulk(kvs: (String, String)*)(implicit G: Monad[G])              = g(self.cfgPutBulk(kvs: _*))
      override def submitBulk[H[_] : Traverse](ms: H[Task])(implicit G: Applicative[G])  = g(self.submitBulk(ms))
      override def submitBulk_[H[_] : Traverse](ms: H[Task])(implicit G: Applicative[G]) = g(self.submitBulk_(ms))
    }
}

object TaskmanApi extends HasLogger {
  def addLogging(self: TaskmanApi[Fx]): TaskmanApi[Fx] =
    new TaskmanApi[Fx] {
      override def cfgPut(k: String, v: String) =
        for {
          (_, dur) <- self.cfgPut(k, v).measureDuration
          _        <- Fx(logger.info(s"Put config {$k=$v} in Taskman in ${dur.toMillis} ms"))
        } yield ()

      override def submit(m: Task) =
        for {
          (id, dur) <- self.submit(m).measureDuration
          _         <- Fx(logger.info(s"Submitted $m to Taskman in ${dur.toMillis} ms"))
        } yield id

      override def getStatus(id: TaskId) =
        for {
          (os, dur) <- self.getStatus(id).measureDuration
          _         <- Fx(logger.info(s"Retrieved $id status as $os from Taskman in ${dur.toMillis} ms"))
        } yield os
    }
}