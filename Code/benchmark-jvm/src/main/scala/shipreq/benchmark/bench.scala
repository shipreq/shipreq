package shipreq.benchmark

// TODO
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import BenchTmp._

@State(Scope.Benchmark)
class FoldBench {

  @Benchmark def demo(): String =
    chars.map(_.length).sum.toString
}
