package shipreq.prop.test

import java.util.concurrent._, atomic.AtomicInteger
import shipreq.prop.Prop
import ParallelExecutor._
import PTest._
import Executor.Data

// TODO data SampleSize = TotalSamples(n) | Fn(qty|%, gensize|%) | PerWorker(sampleSize)

object ParallelExecutor {
  def defaultThreadCount = 1.max(Runtime.getRuntime.availableProcessors - 1)
}

case class ParallelExecutor(workers: Int = defaultThreadCount) extends Executor {

  override def run[A](p: Prop[A], g: Data[A], S: Settings): RunState[A] = {

    val sss = {
      var rem = S.sampleSize.value
      var i = workers
      var v = Vector.empty[SampleSize]
      while(i > 0) {
        val p = rem / i
        v :+= SampleSize(p)
        rem -= p
        i -= 1
      }
      v
    }

    val ai = new AtomicInteger(0)

    def task(worker: Int) = new Callable[RunState[A]] {
      override def call(): RunState[A] = {
        val data = g(sss(worker - 1)).unsafePerformIO()
        testN(p, data, ai.incrementAndGet, S)
      }
    }

    val es: ExecutorService = Executors.newFixedThreadPool(workers)
    val fs = (1 to workers).toList.map(es submit task(_))
    es.shutdown()
    val rss = fs.map(_.get())
    es.awaitTermination(1, TimeUnit.MINUTES)

    def merge(a: RunState[A], b: RunState[A]): RunState[A] = {
      val runs = a.runs max b.runs
      (a.success, b.success) match {
        case (false, true) => RunState(runs, a.result)
        case _             => RunState(runs, b.result)
      }
    }

    val rs = rss.foldLeft(RunState.empty[A])(merge)
    rs
  }
}