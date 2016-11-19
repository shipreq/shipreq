package shipreq.taskman.server.akka

import java.util.concurrent.{TimeUnit, CountDownLatch, Executors}
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import scala.concurrent.duration._
import scala.concurrent._
import scala.slick.jdbc.GetResult
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.ApiOp.SubmitMsg
import shipreq.taskman.api.Msg.DummyMsg
import shipreq.taskman.api.MsgId
import shipreq.taskman.server.ServerImplTestHelpers
import shipreq.taskman.server.SopImpl.{Succeeded, FailAndAbort, ArchiveIntent}
import shipreq.taskman.server.app.Server

class AkkaTest extends Specification with DatabaseTest with NoTimeConversions with HasLogger with ServerImplTestHelpers {

  override def mutex = dbMutexW
  override def wrapTestsInTransaction = false

  implicit val GR_ArchiveIntent: GetResult[ArchiveIntent] =
    implicitly[GetResult[String]] andThen (_.head match {
      case c if c == Succeeded.resultFlag    => Succeeded
      case c if c == FailAndAbort.resultFlag => FailAndAbort
    })

  def lookupHistory(id: MsgId): Option[ArchiveIntent] =
    sql"select result from msg_history where id=${id.value}".as[ArchiveIntent].firstOption

  "Akka integration test" in {
    val es = Executors.newCachedThreadPool()
    implicit val ec = ExecutionContext.fromExecutorService(es)
    val shutdownLatch = new CountDownLatch(1)
    val startTime = System.currentTimeMillis()

    try {
      // start akka
      Future(try
        Server.run(ctx, false)(s => {
          shutdownLatch.await(10, TimeUnit.SECONDS)
          s.shutdown()
          Await.result(s.system.whenTerminated, 10.seconds)
        })
      catch {
        case e: Throwable => log.error(e, "Akka crashed")
      })

      // submit jobs
      val dummy1 = run(SubmitMsg(DummyMsg("#1: Pass immediately")))
      val dummy2 = run(SubmitMsg(DummyMsg("#2: Fail immediately", retryCount = 1, failureMsg = Some("Deliberate fail."))))
      val dummy3 = run(SubmitMsg(DummyMsg("#3: Async pass", async = true)))
      log.debug.z(s"Dummy job ids: ${dummy1.value}, ${dummy2.value}, ${dummy3.value}")

      // wait for results
      val expect = List(Succeeded, FailAndAbort, Succeeded).map(Some(_))
      List(dummy1, dummy2, dummy3).map(lookupHistory) must be_==(expect).eventually(20, 1.second)

    } finally {
      log.info.fmt("Finished in %.3fs", (System.currentTimeMillis() - startTime) / 1000.0)
      shutdownLatch.countDown()
      es.shutdown()
      es.awaitTermination(10, TimeUnit.SECONDS)
      es.shutdownNow()
    }
  }

}
