package shipreq.taskman.server

import java.util.concurrent.{Callable, ExecutorService, Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger
import org.slf4j.{MDC => Slf4jMDC}
import scalaz.syntax.bind._
import shipreq.base.util.FxModule._
import shipreq.taskman.server.logic.Worker

object Async {

  type Scheduler = Worker.AsyncScheduler[java.util.concurrent.Future]

  class CustomThreadFactory(name: String) extends ThreadFactory {
    val count = new AtomicInteger(0)
    val back = Executors.defaultThreadFactory
    override def newThread(r: Runnable): Thread = {
      val r2: Runnable = () => {
        Slf4jMDC.clear()
        r.run()
      }
      val t = back.newThread(r2)
      t setName s"async-$name-${count.incrementAndGet}"
      t
    }
  }

  final case class CallableFx[A](fx: Fx[A]) extends Callable[A] {
    def call(): A = fx.unsafeRun()
  }

  def scheduler(es: ExecutorService): Scheduler =
    new Scheduler {
      override def apply[A](fx: Fx[A]) =
        TaskmanLogging.readMdc >>= { who =>
          val c = TaskmanLogging.writeMdc(s"$who*") >> fx
          Fx(es submit CallableFx(c))
        }
    }

  def newPool(name: String, size: Int): (ExecutorService, Scheduler) = {
    val es = Executors.newFixedThreadPool(size, new CustomThreadFactory(name))
    (es, scheduler(es))
  }
}
