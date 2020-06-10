package shipreq.taskman.server.akka

import doobie.implicits._
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.concurrent._
import scala.concurrent.duration._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Task.DummyTask
import shipreq.taskman.api.TaskId
import shipreq.taskman.server.ServerImplTestHelpers
import shipreq.taskman.server.ServerOpFx.{ArchiveIntent, FailAndAbort, Succeeded}
import shipreq.taskman.server.app.Server
import utest._
import utest.asserts.RetryMax

object AkkaTest extends TestSuite with HasLogger {

  override def tests = Tests {

    "integrationTest" - ServerImplTestHelpers.use { helper =>
      import helper._

      logger.info("Akka integration test starting...")

      def lookupHistory(id: TaskId): Option[ArchiveIntent] =
        (xa ! sql"select result from msg_history where id=${id.value}".query[String].option).map(_.head).map {
          case c if c == Succeeded.resultFlag    => Succeeded
          case c if c == FailAndAbort.resultFlag => FailAndAbort
          case c                                 => sys error s"WTF is '$c'?"
        }

      val es = Executors.newCachedThreadPool()
      implicit val ec = ExecutionContext.fromExecutorService(es)
      val shutdownLatch = new CountDownLatch(1)
      val startTime = System.currentTimeMillis()

      try {

        // start akka
        Future {
          try {
            val s = Server.startAkka(ctx).unsafeRun()
            shutdownLatch.await(12, TimeUnit.SECONDS)
            s.shutdown()
            Await.result(s.system.whenTerminated, 10.seconds)
          } catch {
            case e: Throwable => logger.error("Akka crashed", e)
          }
        }

        // submit jobs
        val dummy1 = runApi(_.submit(DummyTask("#1: Pass immediately")))
        val dummy2 = runApi(_.submit(DummyTask("#2: Fail immediately", retryCount = 1, failureMsg = Some("Deliberate fail."))))
        val dummy3 = runApi(_.submit(DummyTask("#3: Async pass", async = true)))
        logger.debug(s"Dummy job ids: ${dummy1.value}, ${dummy2.value}, ${dummy3.value}")

        // wait for results
        val expect = List(Succeeded, FailAndAbort, Succeeded).map(Some(_))
        implicit val retryMax = RetryMax(10.seconds)
        locally(retryMax) // -Wunused:locals gets it wrong here
        eventually {
          List(dummy1, dummy2, dummy3).map(lookupHistory) == expect
        }

      } finally {
        logger.info("Finished in %.3fs".format((System.currentTimeMillis() - startTime) / 1000.0))
        shutdownLatch.countDown()
        es.shutdown()
        es.awaitTermination(10, TimeUnit.SECONDS)
        es.shutdownNow()
      }

    }
  }
}
