package shipreq.webapp.sampledata

import io.circe.Json
import japgolly.microlibs.testutil.TestUtilImplicits._
import shipreq.webapp.member.project.event.{Event, VerifiedEvent}
import shipreq.webapp.member.protocol.json.v1.Latest._

final case class SampleDataMeta(filename: String,
                                verifiedEvents: Boolean,
                                projectConfigHash: Int,
                                projectContentHash: Int) {

  val decode: Json => Vector[Event] =
    if (verifiedEvents)
      _.as[VerifiedEvent.Seq].getOrThrow().iterator.map(_.event).toVector
    else
      _.as[Vector[Event]].getOrThrow()

  def annotateExceptions[A](a: => A): A =
    try
      a
    catch {
      case t: Throwable =>
        throw new RuntimeException(s"Error in $filename", t)
    }
}

trait SampleDataManifest[D] { self =>
  protected def load(meta: SampleDataMeta): D

  object full {
    def load(size: Int, projectConfigHash: Int, projectContentHash: Int): D =
      self.load(SampleDataMeta(s"sampledata-full-$size.json", false, projectConfigHash, projectContentHash))

    lazy val  `1000`: D = load( 1000,  1452203069, -2018537233)
    lazy val  `2000`: D = load( 2000, -2100690608,  1677984964)
    lazy val  `4000`: D = load( 4000, -1398041708, -1511276617)
    lazy val `10000`: D = load(10000, -1195563770, -1832283493)
  }

  object noReqCodes {
    def load(size: Int, projectConfigHash: Int, projectContentHash: Int): D =
      self.load(SampleDataMeta(s"sampledata-no_req_codes-$size.json", false, projectConfigHash, projectContentHash))

    lazy val  `1000`: D = load( 1000,  -763717055,  1377465625)
    lazy val  `2000`: D = load( 2000, -1376499562,    29101933)
    lazy val  `4000`: D = load( 4000,  -665692199, -1605087212)
    lazy val `10000`: D = load(10000,  -679426977,   729017725)
  }

  object real {
    def load(size: Int, projectConfigHash: Int, projectContentHash: Int): D =
      self.load(SampleDataMeta(s"shipreq-events-real-$size.json", true, projectConfigHash, projectContentHash))

    lazy val `582`: D = load(582, -376644927, 1214255403)
  }

  lazy val all: Vector[D] =
    Vector(
      full      . `1000`,
      full      . `2000`,
      full      . `4000`,
      full      .`10000`,
      noReqCodes. `1000`,
      noReqCodes. `2000`,
      noReqCodes. `4000`,
      noReqCodes.`10000`,
      real      .  `582`,
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
      case ("real",           "582") => real      .  `582`
      case _                         => ???
    }

  private val idFmt = "(.+):(.+)".r

  def byId(id: String): D =
    id match {
      case idFmt(t, s) => byParams(t, s)
    }
}
