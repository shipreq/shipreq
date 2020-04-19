package shipreq.benchmark

trait SampleDataManifest[D] {
  protected def load(filename: String): D

  lazy val `1000` : D = load("shipreq-events-1000.json")
  lazy val `10000`: D = load("shipreq-events-10000.json")

  lazy val all: Vector[D] =
    Vector(`1000`, `10000`)
}
