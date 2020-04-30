package shipreq.webapp.sampledata

final case class SampleDataMeta(filename: String, projectConfigHash: Int, projectContentHash: Int)

trait SampleDataManifest[D] { self =>
  protected def load(meta: SampleDataMeta): D

  object full {
    def load(size: Int, projectConfigHash: Int, projectContentHash: Int): D =
      self.load(SampleDataMeta(s"shipreq-events-full-$size.json", projectConfigHash, projectContentHash))

    lazy val  `1000`: D = load( 1000,   -15975327, -1910444130)
    lazy val  `2000`: D = load( 2000,   454447920, -1851467186)
    lazy val  `4000`: D = load( 4000,   106265614, -1357390324)
    lazy val `10000`: D = load(10000, -1981610195, -1390028356)
  }

  object noReqCodes {
    def load(size: Int, projectConfigHash: Int, projectContentHash: Int): D =
      self.load(SampleDataMeta(s"shipreq-events-no_req_codes-$size.json", projectConfigHash, projectContentHash))

    lazy val  `1000`: D = load( 1000,  251955416, -2078654579)
    lazy val  `2000`: D = load( 2000,  325713993,   682764916)
    lazy val  `4000`: D = load( 4000, 2107620222, -1415010622)
    lazy val `10000`: D = load(10000,  355316401,  -392124681)
  }

  lazy val all: Vector[D] =
    Vector(
      full. `1000`,
      full. `2000`,
      full. `4000`,
      full.`10000`,
      noReqCodes. `1000`,
      noReqCodes. `2000`,
      noReqCodes. `4000`,
      noReqCodes.`10000`,
    )

  def byParams(`type`: String, size: String): D =
    (`type`, size) match {
      case ("full",          "1000") => full      . `1000`
      case ("full",          "2000") => full      . `2000`
      case ("full",          "4000") => full      . `4000`
      case ("full",         "10000") => full      .`10000`
      case ("no_req_codes",  "1000") => noReqCodes. `1000`
      case ("no_req_codes",  "2000") => noReqCodes. `2000`
      case ("no_req_codes",  "4000") => noReqCodes. `4000`
      case ("no_req_codes", "10000") => noReqCodes.`10000`
      case _                         => ???
    }
}
