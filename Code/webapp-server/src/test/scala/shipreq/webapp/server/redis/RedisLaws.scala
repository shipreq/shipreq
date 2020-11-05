package shipreq.webapp.server.redis

import io.circe._
import japgolly.microlibs.utils.ConciseIntSetFormat
import java.time.Instant
import nyaya.gen.Gen
import scalaz.Semigroup
import scalaz.syntax.monad._
import shipreq.webapp.member.data.Project
import shipreq.webapp.member.event.EventOrd.Implicits._
import shipreq.webapp.member.event._
import shipreq.webapp.member.protocol.json.v1.Latest._
import shipreq.webapp.member.protocol.json.v1.PostEvents._
import shipreq.webapp.server.logic.effect.Redis._
import shipreq.webapp.server.test.WebappServerTestUtil._

object RedisLaws {
  import RedisLaw.{Logic, Test}

  type E = VerifiedEvent.Seq
  type S = ProjectSnapshot
  type O = Option[EventOrd.Latest]

  // OPS IN OWN TERMS
  // ================

  val readEvents1 = RedisLaw[O]("readEvents1")(o =>
    readEvents(o) === readEvents(None).map(_.filter(_.ord > o)))

  val readEvents2 = RedisLaw[(O, O)]("readEvents2") { case (a, b) =>
    readEvents(a) ++ readEvents(b) === readEvents(a min b) }

  val writeSnapshot2 = RedisLaw[(S, E, S, E)]("writeSnapshot2") { case (s1, e1, s2, e2) =>
    val s = s1 max s2
    val e = e1 ++ e2
    whenDebugging {
      log(s"[L] writeSnapshot: ", s1)
      log(s"      publishOnly: ", e1)
      log(s"[L] writeSnapshot: ", s2)
      log(s"      publishOnly: ", e2)
      log(s"[R] writeSnapshot: ", s)
      log(s"      publishOnly: ", e)
    }
    writeSnapshot(s1, e1) *> writeSnapshot(s2, e2) <-> writeSnapshot(s, e)
  }

  val writeEvents1 = RedisLaw[(E, E)]("writeEvents1") { case (c, cp) =>
    writeEvents(c, cp) === writeEvents(c -- cp, cp) }

  // This law doesn't hold because we have a property that we don't allow gaps in cached events
  // val writeEvents2 = RedisLaw[(E, E, E, E)]("writeEvents2") { case (c1, cp1, c2, cp2) =>
  //   writeEvents(c1, cp1) *> writeEvents(c2, cp2) =-= writeEvents(c1 ++ c2, cp1 ++ cp2)
  // }

  val publishEvents1 = RedisLaw[(E, E)]("publishEvents1") { case (e1, e2) =>
    publishEvents(e1) *> publishEvents(e2) === publishEvents(e1 ++ e2) }

  // OPS IN TERMS OF OTHER OPS
  // =========================

// This law doesn't hold because we have a property that we don't allow gaps in cached events
//  val readEventsAsRead = RedisLaw[O]("readEventsAsRead")(o =>
//    readEvents(o) === read.map(_.events.filter(_.ord > o)))

  val writeSnapshotAndPublish = RedisLaw[(S, E)]("writeSnapshotAndPublish") { case (s, e) =>
    writeSnapshot(s, e) === publishEvents(e) *> writeSnapshot(s, ∅) }

  val writeEventsAndPublish = RedisLaw[(E, E)]("writeEventsAndPublish") { case (c, cp) =>
    // co = cache only
    // cp = cache & publish
    val co = c -- cp
    whenDebugging {
      log(s"[L] cache            : ", c)
      log(s"    cache and publish: ", cp)
      log(s"[R] publish          : ", cp)
      log(s"[R] cache            : ", co ++ cp)
    }
    writeEvents(co, cp) === publishEvents(cp) *> writeEvents(co ++ cp, ∅)
  }

  val publishEventsViaSnapshot = RedisLaw[E]("publishEventsViaSnapshot")(e =>
    publishEvents(e) === read.flatMap(_.snapshot match {
      case Some(s) => writeSnapshot(s, e).void
      case None    => publishEvents(e)
    }))

  // R/W RELATIONSHIPS
  // =================

  // This law doesn't hold because we have a property that we don't allow gaps in cached events
//  val writeSnapshotAndRead = RedisLaw[(S, E)]("writeSnapshotAndRead") { case (s, e) =>
//    val read2 = read.map(r => ProjectCache(r.snapshot.map(_ max s) orElse Some(s), r.events.filter(_.ord > s.ord)))
//    writeSnapshot(s, e) *> read === (read2 <* writeSnapshot(s, e))
//  }

  // This law doesn't hold because we have a property that we don't allow gaps in cached events
  //  val writeEventsAndRead = RedisLaw[(E, E)]("writeEventsAndRead") { case (c, cp) =>
  //    writeEvents(c, cp) *> read === (read.map(…) <* writeEvents(c, cp))
  //  }

  // ===================================================================================================================

  private def ∅ : E = VerifiedEvent.Seq.empty

  private implicit val eventSeqInstance: Semigroup[VerifiedEvent.Seq] =
    new Semigroup[VerifiedEvent.Seq] {
      override def append(f1: VerifiedEvent.Seq, f2: => VerifiedEvent.Seq) = f1 ++ f2
    }

  private def publishEvents(e: E): Logic[Unit] =
    Logic[Unit](_.publishEvents(_, e))

  private def read: Logic[ProjectCache] =
    Logic[ProjectCache](_.read(_).map(_.getOrThrow()))

  private def readEvents(o: O): Logic[VerifiedEvent.Seq] =
    Logic[VerifiedEvent.Seq](_.readEvents(_, o).map(_.getOrThrow()))

  private def writeEvents(cacheOnly: E, cacheAndPublish: E): Logic[Boolean] =
    Logic[Boolean](_.writeEvents(_, cacheOnly, cacheAndPublish))

  private def writeSnapshot(s: S, publishOnly: E): Logic[Boolean] =
    Logic[Boolean](_.writeSnapshot(_, s, publishOnly))

  // ===================================================================================================================

  var debug = false

  def withDebugging[A](d: Boolean)(f: => A): A = {
    val old = debug
    debug = d
    try
      f
    finally
      debug = old
  }

  def whenDebugging(f: => Unit): Unit =
    if (debug) {
      f
      println()
    }

  def log(prefix: String, e: E): Unit =
    if (e.isEmpty)
      println(prefix + "∅")
    else
      println(prefix + ConciseIntSetFormat(e.map(_.ord.value)))

  def log(prefix: String, s: S): Unit =
    println(prefix + s.ord.value)

  def log(prefix: String, c: ProjectCache): Unit =
    println(s"${prefix}${c.snapshot.fold("∅")(_.ord.value.toString)} + {${ConciseIntSetFormat(c.events.map(_.ord.value))}}")

  // ===================================================================================================================

  def projectSnapshotFromOrd(ord: EventOrd): ProjectSnapshot = {
    val p = Project.empty.copy(name = ord.value.toString)
    ProjectSnapshot(p, ord.asLatest)
  }

  private implicit lazy val encoderProjectSnapshot: Encoder[ProjectSnapshot] =
    Encoder[EventOrd].contramap(_.ord.asEventOrd)

  private implicit lazy val decoderProjectSnapshot: Decoder[ProjectSnapshot] =
    Decoder[EventOrd].map(projectSnapshotFromOrd)

  // ===================================================================================================================

  final case class DataGenerators(genO: Gen[O],
                                  genS: Gen[S],
                                  genE: Gen[E]) {
    val genOO  : Gen[(O, O      )] = Gen.tuple2(genO, genO)
    val genEE  : Gen[(E, E      )] = Gen.tuple2(genE, genE)
    val genSE  : Gen[(S, E      )] = Gen.tuple2(genS, genE)
    val genSESE: Gen[(S, E, S, E)] = Gen.tuple4(genS, genE, genS, genE)
    val genEEEE: Gen[(E, E, E, E)] = Gen.tuple4(genE, genE, genE, genE)
  }

  object DataGenerators {
    private val genZero = Gen.pure(0)

    private def chooseInt(n: Int): Gen[Int] =
      if (n > 0) Gen.chooseInt(n) else genZero

    private val startTime =
      Instant.parse("2020-04-15T00:00:00Z")

    def apply(limit: Int): DataGenerators = {

      val genOrd =
        Gen.chooseInt(limit).map(EventOrd.first + _)

      val genO: Gen[O] =
        Gen.chooseInt(limit + 1).map(i => if (i == 0) None else Some(EventOrd.Latest(i)))

      val genS: Gen[S] =
        genOrd.map(RedisLaws.projectSnapshotFromOrd)

      val genE: Gen[E] =
        for {
          start <- genOrd
          empty <- Gen.chooseInt(8)
          size  <- if (empty == 0) genZero else chooseInt(limit - start.value)
        } yield {
          def events =
            (0 until size).iterator.map { i =>
              val ord = start + i
              VerifiedEvent(ord, Event.ProjectNameSet(ord.value.toString), startTime.plusSeconds(ord.value))
            }
          VerifiedEvent.Seq.empty ++ events
        }

      apply(genO, genS, genE)
    }
  }

  // ===================================================================================================================

  type TestGen = DataGenerators => Gen[Test]

  val testGens: Vector[TestGen] = {
    var results = List.empty[TestGen]

    def add[I](law: RedisLaw[I])(g: DataGenerators => Gen[I]): Unit =
      results ::= (g(_).map(Test(law)(_)))

    add(readEvents1)(_.genO)
    add(readEvents2)(_.genOO)
    add(writeSnapshot2)(_.genSESE)
    add(writeEvents1)(_.genEE)
    add(publishEvents1)(_.genEE)
    add(writeSnapshotAndPublish)(_.genSE)
    add(writeEventsAndPublish)(_.genEE)
    add(publishEventsViaSnapshot)(_.genE)

    results.toVector
  }

  def fromName(str: String): Option[RedisLaw[_]] =
    str match {
      case "readEvents1"              => Some(readEvents1)
      case "readEvents2"              => Some(readEvents2)
      case "writeSnapshot2"           => Some(writeSnapshot2)
      case "writeEvents1"             => Some(writeEvents1)
      case "publishEvents1"           => Some(publishEvents1)
      case "writeSnapshotAndPublish"  => Some(writeSnapshotAndPublish)
      case "writeEventsAndPublish"    => Some(writeEventsAndPublish)
      case "publishEventsViaSnapshot" => Some(publishEventsViaSnapshot)
      case _                          => None
    }

}
