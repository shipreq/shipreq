package shipreq.benchmark.jslib

import scala.scalajs.js
import js.{native, Any => *}
import js.annotation.JSName
import JsBenchmark._

@JSName("Benchmark")
class JsBenchmark(name: String, fn: FN, o: Options = native) extends js.Object {
  def run(options: Options = native): Unit = native
}

object JsBenchmark {

  type FN = js.Function0[*]
  type CB = js.Function1[Event, Unit]

  trait Options extends js.Object {
    var async     : Boolean = native

    /** The delay between test cycles (secs). */
    var defer     : Boolean = native

    /** A flag to indicate that the benchmark clock is deferred. */
    var delay     : Double  = native

    var id        : String  = native

    /** The default number of times to execute a test on a benchmark’s first cycle. */
    var initCount : Int     = native

    /** The time needed to reduce the percent uncertainty of measurement to 1% (secs). */
    var minTime   : Double  = native

    /** The maximum time a benchmark is allowed to run before finishing (secs). */
    var maxTime   : Double  = native

    /** The minimum sample size required to perform statistical analysis. */
    var minSamples: Int     = native

    var name      : String  = native

    var onAbort   : CB = native
    var onComplete: CB = native
    var onCycle   : CB = native
    var onError   : CB = native
    var onReset   : CB = native
    var onStart   : CB = native
  }

  def Options(): Options = js.Object().asInstanceOf[Options]

  trait Event extends js.Object {
    val aborted       : Boolean = native
    val cancelled     : Boolean = native
    val currentTarget : Benchmark  = native
    val result        : *       = native
    val target        : Benchmark  = native
    val timeStamp     : Double  = native
    val `type`        : String  = native
  }

  trait Benchmark extends js.Object {
    val async     : Boolean = native
    val defer     : Boolean = native
    val delay     : Double  = native
    val id        : String  = native
    val initCount : Int     = native
    val minTime   : Double  = native
    val maxTime   : Double  = native
    val minSamples: Int     = native
    val name      : String  = native

    val count  : Int             = native
    val cycles : Int             = native
    val error  : js.UndefOr[*]   = native
    val fn     : js.Function0[*] = native
    val hz     : Double          = native
    val options: Options         = native
    val running: Boolean         = native
    val stats  : Stats           = native
    val times  : Times           = native

    def formatNumber(n: Double): String = native
  }

//  trait Events extends js.Object {
//    val completed: js.Array[*] = native
//    val cycle: js.Array[*] = native
//  }

  trait Stats extends js.Object {
    val deviation : Double           = native
    val mean      : Double           = native
    val moe       : Double           = native
    val rme       : Double           = native
    val sample    : js.Array[Double] = native
    val sem       : Double           = native
    val variance  : Double           = native
  }

  trait Times extends js.Object {
    val cycle     : Double = native
    val elapsed   : Double = native
    val period    : Double = native
    val timeStamp : Double = native
  }

  def fn(f: => Any): FN = () => {
    val a = f
    a.asInstanceOf[js.Any]
  }
  def cb(f: Event => Unit): CB = f
}
