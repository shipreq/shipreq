package shipreq.webapp.server.db

import com.typesafe.scalalogging.StrictLogging
import doobie.ConnectionIO
import japgolly.clearconfig._
import java.time.Duration
import scalaz.~>
import scalaz.syntax.applicative._
import shipreq.base.util.FxModule._
import shipreq.base.util.ThreadUtils

// impure cos on hot path
trait StatRecorder {

  /**
    * @param ip Can be null.
    */
  def recordRequest(rt: ResponseType, ip: String): Unit
}

object StatRecorder extends StrictLogging {

  def apply(runDb: ConnectionIO ~> Fx, cfg: Config): StatRecorder =
    if (cfg.enabled)
      new AsyncBatcher(runDb, cfg.batcher)
    else
      Off

  final case class Config(enabled: Boolean,
                          batcher: AsyncBatcher.Config)

  def config: ConfigDef[Config] =
    ( ConfigDef.getOrUse("enabled", true) |@|
      AsyncBatcher.config
    ) (Config.apply)
      .withPrefix("statRecorder.")

  // ===================================================================================================================

  object Off extends StatRecorder {
    override def recordRequest(rt: ResponseType, ip: String): Unit =
      ()
  }

  // ===================================================================================================================

  object AsyncBatcher {

    final case class Config(submitEvery: Duration,
                            maxAttempts: Int,
                            retryGap   : Duration)

    object Config {
      def default: Config =
        Config(
          submitEvery = Duration.ofSeconds(10),
          maxAttempts = 5,
          retryGap    = Duration.ofSeconds(1),
        )
    }

    def config: ConfigDef[Config] =
      ( ConfigDef.getOrUse("submitEvery", Config.default.submitEvery) |@|
        ConfigDef.getOrUse("maxAttempts", Config.default.maxAttempts) |@|
        ConfigDef.getOrUse("retryGap", Config.default.retryGap)
      ) (Config.apply)
      .withPrefix("batcher.")

    private[this] val noIps = Set.empty[String]

    private[AsyncBatcher] final class State(val responseType: ResponseType) {
      val lock = new AnyRef

      var ips     : Set[String] = _
      var requests: Int         = _

      // Assumes lock held
      def unsafeReset(): Unit = {
        ips      = noIps
        requests = 0
      }

      unsafeReset()
    }
  }

  final class AsyncBatcher(runDb: ConnectionIO ~> Fx, cfg: AsyncBatcher.Config) extends StatRecorder {
    import AsyncBatcher.State

    private[this] val state = ResponseType.values.iterator.map(new State(_)).toArray

    override def recordRequest(rt: ResponseType, ip: String): Unit = {
      val s = state(rt.idx)
      import s._
      lock.synchronized {
        requests += 1
        if (ip != null)
          ips += ip
      }
    }

    private val unsafeSubmitBatch: State => Unit = {

      val betweenRetries: Some[Fx[Unit]] =
        Some(Fx.sleep(cfg.retryGap))

      val retryPolicy: (Int, Throwable) => Option[Fx[Unit]] =
        (attempts, _) => if (attempts > cfg.maxAttempts) None else betweenRetries

      s => {
        var ips      = null: Set[String]
        var requests = 0

        s.lock.synchronized {
          requests = s.requests
          ips      = s.ips
          s.unsafeReset()
        }

        val noStatsToRecord = requests == 0 && ips.isEmpty
        if (!noStatsToRecord) {

          val result =
            runDb(DbInterpreter.logVisitorStats(s.responseType, ips, requests))
              .retryOnException(retryPolicy)
              .attempt
              .unsafeRun()

          if (result.isLeft)
            for (err <- result.left) {

              logger.error("Failed to write stats to database.", err)

              s.lock.synchronized {
                s.ips     ++= ips
                s.requests += requests
              }
            }
        }
      }
    }

    private val submitBatches: Fx[Unit] =
      Fx(state.foreach(unsafeSubmitBatch))

    ThreadUtils.newScheduler("StatRecorder", ThreadUtils.ThreadGroups.scheduledTasks)
      .scheduleAtFixedRate(submitBatches, cfg.submitEvery)
      .addShutdownHook(submitBatches)
  }
}
