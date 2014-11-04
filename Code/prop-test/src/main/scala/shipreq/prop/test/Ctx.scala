package shipreq.prop.test

import scalaz.{-\/, \/, \/-}

final case class GenSize(value: Int) {
  def map(f: Int => Int) = GenSize(f(value))
}
final case class SampleSize(value: Int)  {
  def map(f: Int => Int) = SampleSize(f(value))
}

case class Settings(
  executor   : Executor                      = SingleThreadedExecutor,
  sizeDist   : Seq[(Int, Double \/ GenSize)] = Seq(1 → \/-(GenSize(4)), 1 → -\/(0.2), 8 → -\/(0.8)),
  sampleSize : SampleSize                    = SampleSize(100),
  genSize    : GenSize                       = GenSize(40),
  debug      : Boolean                       = false,
  debugMaxLen: Int                           = 200) {

  lazy val sampleSizeLen = sampleSize.value.toString.length
  lazy val sampleProgressFmt = s"[%${sampleSizeLen}d/${sampleSize.value}] "
}
