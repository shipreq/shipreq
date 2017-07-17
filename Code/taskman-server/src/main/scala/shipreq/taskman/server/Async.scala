package shipreq.taskman.server

import java.util.concurrent.{ExecutorService, Callable, Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger
import org.slf4j.{MDC => SMDC}
import scalaz.syntax.bind._
import shipreq.base.util.FxModule._
import shipreq.base.util.effect.FxE

object Async {

  type Scheduler = Worker.AsyncScheduler[java.util.concurrent.Future]

  class CustomThreadFactory(name: String) extends ThreadFactory {
    val count = new AtomicInteger(0)
    val back = Executors.defaultThreadFactory
    override def newThread(r: Runnable): Thread = {
      val r2: Runnable = () => {
        SMDC.clear()
        r.run()
      }
      val t = back.newThread(r2)
      t setName s"async-$name-${count.incrementAndGet}"
      t
    }
  }

  final case class CallableFx[A](io: Fx[A]) extends Callable[A] {
    def call() = io.unsafeRun()
  }

  def scheduler(es: ExecutorService): Scheduler =
    new Scheduler {
      def apply[A](io: Fx[A]) =
        TaskmanLogging.readMdc >>= { who =>
          val fio = TaskmanLogging.writeMdc(s"$who*") >> io
          FxE(es submit CallableFx(fio))
        }
    }

  def newPool(name: String, size: Int): (ExecutorService, Scheduler) = {
    val es = Executors.newFixedThreadPool(size, new CustomThreadFactory(name))
    (es, scheduler(es))
  }
}
