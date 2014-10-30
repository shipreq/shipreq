package shipreq.base.prop

final case class GenSize(value: Int) {
  def map(f: Int => Int) = GenSize(f(value))
}
final case class SampleSize(value: Int)  {
  def map(f: Int => Int) = SampleSize(f(value))
}

case class Settings(
  sizeDist: Seq[(Double, Double)] = Seq.empty,
  sampleSize: SampleSize          = SampleSize(100),
  genSize: GenSize                = GenSize(40),
  debug: Boolean                  = false,
  debugMaxLen: Int                = 960) {

  lazy val sampleSizeLen = sampleSize.value.toString.length
  lazy val sampleProgressFmt = s"[%${sampleSizeLen}d/${sampleSize.value}] "
}

object Settings {
  val default = Settings()
  object Default {
    implicit def defaultSettings = Settings.default
  }
}

case class Ctx(run: Int, settings: Settings)

object Ctx {
  def single(implicit S: Settings = Settings.default) = Ctx(0, S)
}
