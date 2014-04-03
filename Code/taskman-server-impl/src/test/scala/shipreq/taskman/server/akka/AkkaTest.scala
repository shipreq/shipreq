package shipreq.taskman.server.akka

import java.util.Properties
import java.util.concurrent.{TimeUnit, CountDownLatch, Executors}
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.slick.jdbc.GetResult
import shipreq.base.test.db.specs2.{DatabaseTest, TestDb}
import shipreq.base.util.{JPropertiesValueReader, Props, Logger, RunMode}
import shipreq.taskman.api.ApiOp.SubmitMsg
import shipreq.taskman.api.Msg.DummyMsg
import shipreq.taskman.api.MsgId
import shipreq.taskman.server.Sql.{Succeeded, FailAndAbort, ArchiveIntent}
import shipreq.taskman.server.app.Server
import shipreq.taskman.server.{ServerImplTestHelpers, TaskmanCtx}

class AkkaTest extends Specification with DatabaseTest with NoTimeConversions with Logger with ServerImplTestHelpers {

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
    val props = Props.loadUsingStandardStrategy(RunMode.Test)(new Properties)
    val propsR = JPropertiesValueReader(props)
    val ctx: TaskmanCtx = new TaskmanCtx(TestDb.slick, props, propsR) {
      override def fromDb = propsR
    }

    try {
      // start akka
      Future(try
        Server.run(ctx)(s => {
          shutdownLatch.await(10, TimeUnit.SECONDS)
          s.shutdown()
          s.system.awaitTermination(10.seconds)
        })
      catch {
        case e: Throwable => log.error("Akka crashed", e)
      })

      // submit jobs
      val dummy1 = run(SubmitMsg(DummyMsg("#1: Pass immediately")))
      val dummy2 = run(SubmitMsg(DummyMsg("#2: Fail immediately", retryCount = 1, failureMsg = Some("Deliberate fail."))))
      log.debug("Dummy job ids: {}, {}", dummy1.value, dummy2.value)

      // wait for results
      val expect = List(Succeeded, FailAndAbort).map(Some(_))
      List(dummy1, dummy2).map(lookupHistory) must be_==(expect).eventually(20, 1.second)

    } finally {
      shutdownLatch.countDown()
      es.shutdown()
      es.awaitTermination(10, TimeUnit.SECONDS)
      es.shutdownNow()
    }
  }

}
