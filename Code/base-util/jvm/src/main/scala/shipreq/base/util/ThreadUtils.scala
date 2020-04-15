package shipreq.base.util

import cats.effect.{ContextShift, IO, Timer}
import com.typesafe.scalalogging.Logger
import java.util.concurrent.{Executors, LinkedBlockingQueue, ThreadFactory, ThreadPoolExecutor, TimeUnit}
import java.time.Duration
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext

object ThreadUtils {
  import FxModule._

  object ThreadGroups {
    val scheduledTasks = new ThreadGroup("ScheduledTasks")
    val shutdown       = new ThreadGroup("Shutdown")
  }

  def runOnShutdown[A](name: String, a: => A)(implicit ev: A =:= Unit): Unit =
    runOnShutdownFx(name, Fx(ev(a)))

  def runOnShutdownFx(name: String, fx: Fx[Unit]): Unit = {
    val t = new Thread(ThreadGroups.shutdown, fx.toJavaRunnable, "shutdown-" + name)
    Runtime.getRuntime.addShutdownHook(t)
  }

  def newThreadFactory(groupName: String): ThreadFactory = {
    val group  = new ThreadGroup(groupName)
    val count  = new AtomicInteger(0)
    val prefix = groupName + "-thread-"
    new ThreadFactory {
      override def newThread(r: Runnable) = {
        val name = prefix + count.incrementAndGet()
        val t    = new Thread(group, r, name)
        t.setDaemon(true)
        t
      }
    }
  }

  def newThreadPoolExecutor(threads: Int, name: String): ThreadPoolExecutor =
    newThreadPoolExecutor(threads, newThreadFactory(name))

  def newThreadPoolExecutor(threads: Int, threadFactory: ThreadFactory): ThreadPoolExecutor = {
    val e = new ThreadPoolExecutor(threads,
      threads,
      0L,
      TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable],
      threadFactory)
    e.prestartAllCoreThreads()
    e
  }

  implicit class ExecutionContextExt(private val ec: ExecutionContext) extends AnyVal {

    def onUncaughtError(f: Throwable => Unit): ExecutionContext =
      new ExecutionContext {
        def execute(r: Runnable): Unit =
          ec.execute { () =>
            try
              r.run()
            catch {
              case e: Throwable => f(e)
            }
          }
        def reportFailure(cause: Throwable): Unit =
          ec.reportFailure(cause)
      }

    def logUncaughtErrors(l: Logger): ExecutionContext =
      onUncaughtError(e => l.error("Uncaught error in ExecutionContext: {}", e.toString, e))
  }

  // ===================================================================================================================

  def newThreadPool(threadGroupName: String, logger: Logger): ThreadPool =
    new ThreadPool(threadGroupName, newThreadFactory(threadGroupName), logger)

  final class ThreadPool(threadGroupName: String, threadFactory: ThreadFactory, logger: Logger) {
    def withThreads(n: Int) = ThreadPool2(threadGroupName, newThreadPoolExecutor(n, threadFactory), logger)
    def withMaxThreads() = withThreads(Runtime.getRuntime.availableProcessors())
  }

  final case class ThreadPool2(threadGroupName: String, threadPoolExecutor: ThreadPoolExecutor, logger: Logger) {
    def executionContext: ExecutionContext =
      ExecutionContext.fromExecutor(threadPoolExecutor).logUncaughtErrors(logger)

    def contextShift: ContextShift[IO] =
      IO.contextShift(executionContext)

    def timer: Timer[IO] =
      IO.timer(executionContext)

    def shutdownFx: Fx[Unit] =
      Fx(threadPoolExecutor.shutdown())

    def autoShutdown(): ThreadPool2 = {
      runOnShutdownFx(threadGroupName, shutdownFx)
      this
    }
  }

  // ===================================================================================================================

  def newScheduler(threadName: String, threadGroup: ThreadGroup): Scheduler =
    new Scheduler(threadName, threadGroup)

  final class Scheduler(threadName: String, threadGroup: ThreadGroup) {
    private val es = Executors.newSingleThreadScheduledExecutor(new Thread(threadGroup, _, threadName))

    def scheduleAtFixedRate[A](fx: Fx[A], period: Duration): Scheduler =
      scheduleAtFixedRate(fx, period, period)

    def scheduleAtFixedRate[A](fx: Fx[A], initialDelay: Duration, period: Duration): Scheduler = {
      es.scheduleAtFixedRate(fx.toJavaRunnable, initialDelay.toMillis, period.toMillis, TimeUnit.MILLISECONDS)
      this
    }

    def addShutdownHook(fx: Fx[Unit]): Scheduler = {
      runOnShutdownFx(threadName, fx)
      this
    }
  }

}
