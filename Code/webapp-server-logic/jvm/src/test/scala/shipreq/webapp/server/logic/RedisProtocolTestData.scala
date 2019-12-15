package shipreq.webapp.server.logic

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Instant
import java.util.concurrent.Executors
import nyaya.gen.{Gen, GenCtx, GenSize, ThreadNumber}
import scala.annotation.tailrec
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.base.util.BinaryData

/* SBT, being the complete piece of fucking garbage that it is, can't run this because it doesn't generate the classpath
 * properly for some reason.
 *
 * As a workaround, add this to a test:
 *
 *     'ffs - RedisProtocolTestData.main(Array.empty)
 *
 */
object RedisProtocolTestData {
  import shipreq.webapp.base.protocol.json.v1.PostEvents._
  import RedisProtocol._

  private[this] val RowEvent          = "event"
  private[this] val RowEventBinary    = "binary.event"
  private[this] val RowSnapshotBinary = "binary.snapshot"

  final case class Row(eventJson: Json,
                       eventBinary: BinaryData,
                       snapshotBinary: BinaryData) {

    lazy val parseEventJson =
      eventJson.as[VerifiedEvent]

    lazy val parseEventBinary =
      picklerEvent.decode(eventBinary)

    lazy val parseSnapshotBinary =
      picklerProjectSnapshot.decode(snapshotBinary)
  }

  implicit val decoderBinaryData: Decoder[BinaryData] =
    Decoder.decodeString.emapTry(s => Try(BinaryData.fromHex(s)))

  implicit val encoderBinaryData: Encoder[BinaryData] =
    Encoder.encodeString.contramap(_.hex)

  implicit val decoderRow: Decoder[Row] =
    Decoder.forProduct3(RowEvent, RowEventBinary, RowSnapshotBinary)(Row.apply)

  implicit val encoderRow: Encoder[Row] =
    Encoder.forProduct3(RowEvent, RowEventBinary, RowSnapshotBinary)(a => (a.eventJson, a.eventJson, a.snapshotBinary))

  def load(): Vector[Row] =
    decode[Vector[Row]](readResourceFile("RedisProtocolTestData.json")).needRight

  def main(args: Array[String]): Unit = {
    val _ = picklerEvent // Ensure the classpath is correct - thanks SBT

    val events = generateEvents()
    val rows = makeRows(events)

    val filename = "/tmp/RedisProtocolTestData.json"
    println(s"Writing to $filename")
    val content = rows.map(prettyPrintJson(_).indent("  ")).mkString("[\n", ",\n", "\n]")
    writeFile(filename, content)
    printf("File size: %,d\n", content.length)

    println("Done")
  }

  def generateEvents(): List[Event] = {
    println("Generating events...")

    val genRetries  = 200
    val parAttempts = 10
    val threadRatio = 1.2
    val threads     = (Runtime.getRuntime.availableProcessors() * threadRatio).ceil.toInt
    val goodSeed    = -4647193967173812310L

    val seeds = Iterator.single(goodSeed) ++ Gen.long.samples()

    def run() = {

      val genCtx = GenCtx(GenSize.Default, ThreadNumber(0))
      val seed = seeds.synchronized(seeds.next())
      genCtx.setSeed(seed)

      @tailrec
      def go(pending: Set[EventName], events: List[Event], ps: NonEmptyVector[Project], retries: Int): List[Event] =
        NonEmptySet.option(pending) match {
          case Some(pendingNES) =>

            val s  = (ps.last, EventOrd.first)
            Try(RandomEventStream.verifiedEventOfTypes(pendingNES).run(s).samplesUsing(genCtx).next()) match {
              case Success(r) =>
                // print(s"${pending.size}... ")
                val e  = r._2.event
                val p2 = r._1._1
                go(pending - EventName(e), e :: events, ps :+ p2, retries)
              case Failure(e) =>
                if (retries > 0 && events.nonEmpty) {
                  val last = events.head
                  go(pending + EventName(last), events.tail, NonEmptyVector force ps.init, retries - 1)
                } else
                  throw e
            }


          case None =>
            println(s"ok (seed:${seed}L)")
            events.reverse
        }

      val ((initProject, _), initEvents) = RandomEventStream.initialEvents.samplesUsing(genCtx).next()

      go(
        EventName.all.whole -- initEvents.map(v => EventName(v.event)),
        initEvents.reverseIterator.map(_.event).toList,
        NonEmptyVector one initProject,
        genRetries)
    }

    val es = Executors.newFixedThreadPool(threads)
    try {
      implicit val ec = ExecutionContext.fromExecutor(es)

      def submit() = Future(Try(run()))

      def parallelAttempt() = {
        print('.')
        val fs = List.fill(threads)(submit())
        val rs = Await.result(Future.sequence(fs), 30.seconds.asFiniteDuration)
        rs.find(_.isSuccess).getOrElse(rs.head)
      }

      val result = Iterator.fill(parAttempts)(parallelAttempt()).find(_.isSuccess).getOrElse(parallelAttempt()).get
      println()
      result

    } finally
      es.shutdown()
  }

  def makeRows(es: List[Event]): List[Row] = {
    println("Creating rows...")
    type S = (Option[Redis.ProjectSnapshot], Instant, List[Row])
    val zero: S = (None, Instant.parse("2019-09-27T03:10:41.423Z"), Nil)
    val result: S =
      es.foldLeft(zero) { (prev, e) =>
        val ord  = prev._1.fold(EventOrd.first)(x => EventOrd(x.ord.value) + 1)
        val time = prev._2.plusNanos(e.##.abs)
        val ve   = VerifiedEvent(ord, e, time)
        val p1   = prev._1.fold(Project.empty)(_.project)
        val p2   = applyVerifiedEventSuccessfully(p1, ve)
        val ps   = Redis.ProjectSnapshot(p2, ord.asLatest)
        val row  = makeRow(ve, ps)
        (Some(ps), time, row :: prev._3)
      }
    result._3.reverse
  }

  def makeRow(ve: VerifiedEvent, ps: Redis.ProjectSnapshot): Row =
    Row(ve.asJson, picklerEvent.encode(ve), picklerProjectSnapshot.encode(ps))

  def prettyPrintJson(r: Row): String =
    s"""|{
        |  "$RowEvent" : ${r.eventJson.noSpacesSortKeys},
        |  "$RowEventBinary" : ${r.eventBinary.asJson.noSpacesSortKeys},
        |  "$RowSnapshotBinary" : ${r.snapshotBinary.asJson.noSpacesSortKeys}
        |}
        |""".stripMargin.trim
}