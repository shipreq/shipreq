package shipreq.base.prop

import shipreq.base.util.TaggedTypes.TaggedInt

final case class GenSize(value: Int) extends TaggedInt {
  def map(f: Int => Int) = GenSize(f(value))
}
final case class SampleSize(value: Int) extends TaggedInt  {
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

case class Ctx[A](a: A, run: Int, settings: Settings) {
  def map[B](f: A => B) = Ctx[B](f(a), run, settings)
}

object Ctx {
  def single[A](a: A)(implicit S: Settings = Settings.default) = Ctx(a, 0, S)
}
