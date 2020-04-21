package shipreq.webapp.sampledata

trait SampleDataManifest[D] {
  protected def load(filename: String): D

  lazy val  `1000`: D = load("shipreq-events-1000.json")
  lazy val  `2000`: D = load("shipreq-events-2000.json")
  lazy val `10000`: D = load("shipreq-events-10000.json")

  lazy val all: Vector[D] =
    Vector(`1000`, `2000`, `10000`)

  val byName: String => D = {
    case  "1000" =>  `1000`
    case  "2000" =>  `2000`
    case "10000" => `10000`
  }
}
