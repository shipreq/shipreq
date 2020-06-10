package shipreq.taskman.server

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Callable, ExecutorService, Executors, ThreadFactory}
import org.slf4j.MDC
import shipreq.base.util.FxModule._
import shipreq.base.util.log.MdcUtil
import shipreq.taskman.server.logic.Worker

object Async {

  type Scheduler = Worker.AsyncScheduler[java.util.concurrent.Future]

  private final class CustomThreadFactory(name: String) extends ThreadFactory {
    val count = new AtomicInteger(0)
    val underlying = Executors.defaultThreadFactory
    override def newThread(r: Runnable): Thread = {
      val r2: Runnable = () => {
        MDC.clear()
        r.run()
      }
      val t = underlying.newThread(r2)
      t.setName(s"async-$name-${count.incrementAndGet}")
      t
    }
  }

  private final case class CallableFx[A](fx: Fx[A]) extends Callable[A] {
    def call(): A = fx.unsafeRun()
  }

  def scheduler(es: ExecutorService): Scheduler =
    new Scheduler {
      override def apply[A](body: Fx[A]): Fx[java.util.concurrent.Future[A]] =
        for {
          bodyAllMdc <- MdcUtil.preserve(body)
          future     <- Fx(es.submit(CallableFx(bodyAllMdc)))
        } yield future
    }

  def newPool(name: String, size: Int): (ExecutorService, Scheduler) = {
    val es = Executors.newFixedThreadPool(size, new CustomThreadFactory(name))
    (es, scheduler(es))
  }
}
