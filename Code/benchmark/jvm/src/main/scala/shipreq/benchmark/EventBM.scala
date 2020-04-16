package shipreq.benchmark

/* > jmh:run -prof gc Event
 *
 * =====================================================================================================================
 * ApplyEvent on KleisliEither
 * =====================================================================================================================
 *
 * Benchmark                                            Mode  Cnt        Score        Error   Units
 * EventBM.trusted                                     thrpt  200      184.909 ±      1.185   ops/s
 * EventBM.trusted:·gc.alloc.rate                      thrpt  200      317.939 ±      2.043  MB/sec
 * EventBM.trusted:·gc.alloc.rate.norm                 thrpt  200  1803316.533 ±   1432.240    B/op
 * EventBM.trusted:·gc.churn.PS_Eden_Space             thrpt  200      315.296 ±     42.019  MB/sec
 * EventBM.trusted:·gc.churn.PS_Eden_Space.norm        thrpt  200  1792034.528 ± 239749.208    B/op
 * EventBM.trusted:·gc.churn.PS_Survivor_Space         thrpt  200        0.015 ±      0.006  MB/sec
 * EventBM.trusted:·gc.churn.PS_Survivor_Space.norm    thrpt  200       84.027 ±     34.068    B/op
 * EventBM.trusted:·gc.count                           thrpt  200      159.000               counts
 * EventBM.trusted:·gc.time                            thrpt  200      540.000                   ms
 * EventBM.untrusted                                   thrpt  200      185.824 ±      1.295   ops/s
 * EventBM.untrusted:·gc.alloc.rate                    thrpt  200      319.230 ±      2.214  MB/sec
 * EventBM.untrusted:·gc.alloc.rate.norm               thrpt  200  1801732.252 ±    757.105    B/op
 * EventBM.untrusted:·gc.churn.PS_Eden_Space           thrpt  200      322.330 ±     50.136  MB/sec
 * EventBM.untrusted:·gc.churn.PS_Eden_Space.norm      thrpt  200  1826033.745 ± 285459.254    B/op
 * EventBM.untrusted:·gc.churn.PS_Survivor_Space       thrpt  200        0.015 ±      0.007  MB/sec
 * EventBM.untrusted:·gc.churn.PS_Survivor_Space.norm  thrpt  200       82.252 ±     36.959    B/op
 * EventBM.untrusted:·gc.count                         thrpt  200      143.000               counts
 * EventBM.untrusted:·gc.time                          thrpt  200      430.000                   ms
 *
 * =====================================================================================================================
 * ApplyEvent on StateEither (using folds instead of Util.mapReduce)
 * =====================================================================================================================
 *
 * Benchmark                                            Mode  Cnt        Score        Error   Units
 * EventBM.trusted                                     thrpt  200      180.016 ±      1.463   ops/s
 * EventBM.trusted:·gc.alloc.rate                      thrpt  200      324.268 ±      2.632  MB/sec
 * EventBM.trusted:·gc.alloc.rate.norm                 thrpt  200  1889221.177 ±    300.034    B/op
 * EventBM.trusted:·gc.churn.PS_Eden_Space             thrpt  200      326.514 ±     31.163  MB/sec
 * EventBM.trusted:·gc.churn.PS_Eden_Space.norm        thrpt  200  1905403.521 ± 183543.424    B/op
 * EventBM.trusted:·gc.churn.PS_Survivor_Space         thrpt  200        0.021 ±      0.007  MB/sec
 * EventBM.trusted:·gc.churn.PS_Survivor_Space.norm    thrpt  200      124.104 ±     39.407    B/op
 * EventBM.trusted:·gc.count                           thrpt  200      226.000               counts
 * EventBM.trusted:·gc.time                            thrpt  200      786.000                   ms
 * EventBM.untrusted                                   thrpt  200      179.902 ±      1.340   ops/s
 * EventBM.untrusted:·gc.alloc.rate                    thrpt  200      323.808 ±      2.419  MB/sec
 * EventBM.untrusted:·gc.alloc.rate.norm               thrpt  200  1887708.934 ±    954.913    B/op
 * EventBM.untrusted:·gc.churn.PS_Eden_Space           thrpt  200      323.844 ±     25.842  MB/sec
 * EventBM.untrusted:·gc.churn.PS_Eden_Space.norm      thrpt  200  1891282.019 ± 152128.634    B/op
 * EventBM.untrusted:·gc.churn.PS_Survivor_Space       thrpt  200        0.023 ±      0.007  MB/sec
 * EventBM.untrusted:·gc.churn.PS_Survivor_Space.norm  thrpt  200      132.631 ±     42.278    B/op
 * EventBM.untrusted:·gc.count                         thrpt  200      237.000               counts
 * EventBM.untrusted:·gc.time                          thrpt  200      674.000                   ms
 *
 * =====================================================================================================================
 * ApplyEvent on StateEither (using Util.mapReduce)
 * =====================================================================================================================
 *
 * Benchmark                                            Mode  Cnt        Score        Error   Units
 * EventBM.trusted                                     thrpt  200      180.647 ±      1.483   ops/s
 * EventBM.trusted:·gc.alloc.rate                      thrpt  200      324.163 ±      2.653  MB/sec
 * EventBM.trusted:·gc.alloc.rate.norm                 thrpt  200  1881990.876 ±    381.659    B/op
 * EventBM.trusted:·gc.churn.PS_Eden_Space             thrpt  200      328.306 ±     24.792  MB/sec
 * EventBM.trusted:·gc.churn.PS_Eden_Space.norm        thrpt  200  1908421.436 ± 146036.773    B/op
 * EventBM.trusted:·gc.churn.PS_Survivor_Space         thrpt  200        0.019 ±      0.007  MB/sec
 * EventBM.trusted:·gc.churn.PS_Survivor_Space.norm    thrpt  200      111.505 ±     40.266    B/op
 * EventBM.trusted:·gc.count                           thrpt  200      227.000               counts
 * EventBM.trusted:·gc.time                            thrpt  200      644.000                   ms
 * EventBM.untrusted                                   thrpt  200      180.889 ±      1.475   ops/s
 * EventBM.untrusted:·gc.alloc.rate                    thrpt  200      324.635 ±      2.645  MB/sec
 * EventBM.untrusted:·gc.alloc.rate.norm               thrpt  200  1882257.987 ±    411.760    B/op
 * EventBM.untrusted:·gc.churn.PS_Eden_Space           thrpt  200      331.440 ±     23.108  MB/sec
 * EventBM.untrusted:·gc.churn.PS_Eden_Space.norm      thrpt  200  1924120.181 ± 135279.275    B/op
 * EventBM.untrusted:·gc.churn.PS_Survivor_Space       thrpt  200        0.021 ±      0.007  MB/sec
 * EventBM.untrusted:·gc.churn.PS_Survivor_Space.norm  thrpt  200      118.920 ±     37.997    B/op
 * EventBM.untrusted:·gc.count                         thrpt  200      253.000               counts
 * EventBM.untrusted:·gc.time                          thrpt  200      605.000                   ms
 */

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ApplyEvent

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class EventBM {

  val events = SampleData.`1000`.verifiedEvents
  val pe = Project.empty

  val ae_trusted = ApplyEvent.trusted
  val ae_untrusted = ApplyEvent.untrusted

  def go(ae: ApplyEvent): Project =
    ae.applyVerified(events)(pe) match {
      case \/-(p) => p
      case -\/(e) => println(e); sys.error(e)
    }

  @Benchmark def untrusted = go(ae_untrusted)
  @Benchmark def trusted   = go(ae_trusted)
}