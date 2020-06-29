package shipreq.webapp.server.redis

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.FileUtils
import java.time.Duration
import java.util.concurrent.atomic.AtomicInteger
import nyaya.gen._
import scala.collection.View
import scala.reflect.ClassTag
import scalaz.{Equal, \/}
import shipreq.base.test.JsonTestUtil._
import shipreq.base.test._
import shipreq.base.util.FxModule._
import shipreq.base.util._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.event._
import shipreq.webapp.server.logic.Redis._
import shipreq.webapp.server.redis.RedisLaw.Test
import shipreq.webapp.server.redis.RedisLawTester._
import shipreq.webapp.server.redis.RedisLaws.DataGenerators
import shipreq.webapp.server.test.WebappServerTestUtil._
import sourcecode.Line

object RedisLawTester {
  import RedisLaw.Test

  final case class Settings(

      /** How many reps to run before concluding success. */
      reps: Int,

      /** Initial seed for the entire test */
      seed: () => Long,

      /** Random data generators given a max-event limit */
      gens: Int => DataGenerators,

      /** Maximum number of events to generate per rep. */
      maxEvents: Int => Int,

      /** How often to simulate Redis eviction. Eg 0.1 means 10% chance per rep to evict. */
      evict: Double,

      /** Given a number of failed cmds, whether shrinking should be attempted and if so, what the breadth limit is. */
      shrinkLimit: Int => Option[Int],

      shrinkMaxDur: Duration,

      verbose: Boolean) {

    def withSeed(s: Long): Settings =
      copy(seed = () => s)

    def withShrinkLimit(l: Int): Settings =
      copy(shrinkLimit = _ => Some(l))
  }

  object Settings {
    val default: Settings =
      apply(
        reps           = 100,
        seed           = () => Gen.long.sample(),
        gens           = DataGenerators.apply,
        maxEvents      = rep => (rep / 3 + 1).min(6),
        evict          = 0.1,
        shrinkLimit    = i => Some(if (i > 100) 1 else if (i > 50) 2 else 4),
        shrinkMaxDur   = Duration.ofSeconds(30),
        verbose        = false)
  }

  // ===================================================================================================================

  final case class State[Alg1        <: ProjectAlgebra[Fx],
                         Alg2        <: ProjectAlgebra[Fx]]
                        (id1          : ProjectId,
                         id2          : ProjectId,
                         alg1         : Alg1,
                         alg2         : Alg2,
                         evictSnapshot: Fx[Unit],
                         publish      : Fx[Unit])

  // ===================================================================================================================

  private[RedisLawTester] final class Listener(id: ProjectId, alg: ProjectAlgebra[Fx]) {
    import shipreq.webapp.server.logic.Redis.ListenerError

    private val state = new collection.mutable.ArrayBuffer[VerifiedEvent]

    val init  = Fx(alg.subscribe(id, add))
    val get   = Fx(synchronized(state.toList))
    val clear = Fx(synchronized(state.clear()))

    private def add(e: ListenerError \/ VerifiedEvent): Fx[Unit] =
      Fx {
        synchronized {
          //      val before = get()
          state += e.getOrThrow()
          //      val after = get()
          //      val p =  if (r.toString.contains("Memory")) "<<M>>" else "<<R>>"
          //      println(s"${p} ${before} + ${e} --> ${after}")
        }
      }
  }

  // ===================================================================================================================

  sealed trait Cmd {
    def name: String
  }
  object Cmd {
    case object EvictSnapshot extends Cmd {
      override def name = "EvictSnapshot"
    }
    final case class RunTest(test: Test) extends Cmd {
      override def name = test.name
    }
  }

  private object JsonCodecs {

    implicit val decoderTest: Decoder[Test] =
      Decoder.instance { c =>

        def read[I](law: RedisLaw[I]) =
          c.get("input")(law.inputJsonDecoder).map(Test(law)(_))

        c.get[String]("name").flatMap { name =>
          RedisLaws.fromName(name) match {
            case Some(law) => read(law)
            case None      => Left(DecodingFailure("Unknown law: " + name, c.history))
        }
      }
    }

    implicit val encoderTest: Encoder[Test] =
      Encoder.instance { t =>
        Json.obj(
          "name"  -> t.name.asJson,
          "input" -> t.inputJson)
      }

    implicit val decoderCmdRunTest: Decoder[Cmd.RunTest] =
      Decoder[Test].map(Cmd.RunTest.apply)

    implicit val encoderCmdRunTest: Encoder[Cmd.RunTest] =
      Encoder[Test].contramap(_.test)

    implicit val decoderCmd: Decoder[Cmd] = JsonUtil.decodeSumBySoleKey {
      case ("evictSnapshot", _) => Right(Cmd.EvictSnapshot)
      case ("runTest"      , c) => c.as[Cmd.RunTest]
    }

    implicit val encoderCmd: Encoder[Cmd] = Encoder.instance {
      case Cmd.EvictSnapshot => Json.obj("evictSnapshot" -> ().asJson)
      case a: Cmd.RunTest    => Json.obj("runTest"       -> a.asJson)
    }
  }

  import JsonCodecs._

  def parseCmds(json: String): Vector[Cmd] =
    decodeOrThrow[Vector[Cmd]](json)

  private[RedisLawTester] val reproductionsGenerated = new AtomicInteger(0)

  // ===================================================================================================================

  final case class Failure[A](name: String, lhs: A, rhs: A)(implicit val equality: Equal[A], line: Line) {
    def print(): Unit = {
      try {
        assertEq(name, expect = lhs, actual = rhs)
      } catch {
        case _: Throwable =>
      }
      ()
    }
  }

  final class Failures {
    private val lock = new AnyRef
    private var _failures = List.empty[Failure[_]]

    def add[A](f: Failure[A]) = lock.synchronized(_failures ::= f)
    def get() = lock.synchronized(_failures)
    def isEmpty() = get().isEmpty
    def nonEmpty() = get().nonEmpty
    def print() = get().foreach(_.print())

    def failureSummary(name: => String = null) =
      get().map(_.name).mkString(Option(name).getOrElse("RedisLaws failed") + ": ", ", ", "")
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final case class RedisLawTester[F <: ProjectAlgebra[Fx], G <: ProjectAlgebra[Fx]](newState: Fx[State[F, G]]) {

  private val state = newState.unsafeRun()
  import state.{evictSnapshot, publish}

  def id1  = state.id1
  def id2  = state.id2
  def alg1 = state.alg1
  def alg2 = state.alg2

  private val listener1 = new RedisLawTester.Listener(id1, alg1)
  private val listener2 = new RedisLawTester.Listener(id2, alg2)

  listener1.init.unsafeRun()
  listener2.init.unsafeRun()

  private implicit val failures = new Failures

  protected implicit val retryMax = utest.asserts.RetryMax(5000.millis.asFiniteDuration)
  protected implicit val retryInterval = utest.asserts.RetryInterval(5.millis.asFiniteDuration)

  private def testEq[A](name: => String, lhs: A, rhs: A)(implicit e: Equal[A], q: Line): Unit =
    if (!e.equal(lhs, rhs))
      failures add Failure(name, lhs, rhs)

  private def assertState(name: String): Unit = {
    // Test state: read
    val s1 = alg1.read(id1).unsafeRun().getOrThrow()
    val s2 = alg2.read(id2).unsafeRun().getOrThrow()
    testEq(s"$name -> .read", s1, s2)

    // Test state: events
    val e1 = alg1.readEvents(id1, None).unsafeRun().getOrThrow()
    val e2 = alg2.readEvents(id2, None).unsafeRun().getOrThrow()
    testEq(s"$name -> .readEvents", e1, e2)

    // Test invariants
    val s = s1
    if (s.events.size > 1) {
      val ords = s.events.iterator.map(_.ord.value).toList
      testEq("no event gaps allowed", ords, Util.partitionConsecutive(ords)._1)
    }
    if (s.snapshot.isDefined && s.events.nonEmpty)
      testEq("first event",
        s.events.min.ord.value,
        s.snapshot.get.ord.value + 1)
  }

  private def assertTest(test: Test): Unit = {
    // Prepare
    listener1.clear.unsafeRun()
    listener2.clear.unsafeRun()

    // Run
    val equation = test.equation
    val o1 = equation.lhs.run(alg1, id1).unsafeRun()
    val o2 = equation.rhs.run(alg2, id2).unsafeRun()
    publish.unsafeRun()

    // Test state
    assertState(test.name)

    // Test laws
    def p1 = listener1.get.unsafeRun().iterator.map(_.ord).toSet
    def p2 = listener2.get.unsafeRun().iterator.map(_.ord).toSet
    testEq(s"${test.name} result", o1, o2)(equation.equality, implicitly)
    try
      utest.eventually(p1 === p2)
    catch {
      case _: Throwable => testEq(s"${test.name} -> published", p1, p2)
    }
  }

  private def generateCmds(gens            : Int => DataGenerators,
                           reps            : Int,
                           seed            : Long,
                           genEvictSnapshot: Gen[Boolean]): Iterator[Cmd] = {
    val genCtx = GenCtx(GenSize.Default, ThreadNumber(0))
    genCtx.setSeed(seed)
    val itEvictSnapshot = genEvictSnapshot.samplesUsing(genCtx)
    1.to(reps).iterator.flatMap { rep =>

      // Create a cmd for each law
      val gs     = gens(rep)
      val tests0 = RedisLaws.testGens.iterator.map[Cmd](g => Cmd.RunTest(g(gs).samplesUsing(genCtx).next())).toList
      val tests  = Gen.shuffle(tests0).samplesUsing(genCtx).next()

      // Maybe chuck in an eviction too
      if (rep != 1 && itEvictSnapshot.next())
        Cmd.EvictSnapshot :: tests
      else
        tests
    }
  }

  def apply(cmd: Cmd): Unit =
    cmd match {
      case Cmd.RunTest(t) =>
        assertTest(t)

      case Cmd.EvictSnapshot =>
        evictSnapshot.unsafeRunSync()
        assertState("evictSnapshot")
    }

  def assertApplyOk(cmd: Cmd)(implicit l: Line): Unit =
    assertApplyOk(null, cmd)

  def assertApplyOk(name: => String, cmd: Cmd)(implicit l: Line): Unit = {
    apply(cmd)
    if (failures.nonEmpty()) {
      failures.print()
      fail(failures.failureSummary(name))
    }
  }

  def testAllLaws(settings: Settings)(implicit l: Line): String = {
    import settings.{seed => _, _}
    val startTime  = System.nanoTime()
    val seed       = settings.seed()
    val genEvictSS = Gen.chooseDouble(0, 1).map(_ < evict)
    val cmds       = generateCmds(gens, reps, seed, genEvictSS).toArray
    val lawCount   = RedisLaws.testGens.length

    if (verbose)
      println(s"Testing $lawCount Redis laws $reps times each for a total of ${cmds.length} tests, with seed $seed")

    for (i <- cmds.indices) {
      val cmd = cmds(i)

      if (verbose)
        cmd match {
          case Cmd.RunTest(t)    => printf("\n[%4d] %-24s: %s\n", i + 1, t.name, t.input.toString.take(180))
          case Cmd.EvictSnapshot => printf("\n[%4d] Evict snapshot\n", i + 1)
        }

      apply(cmd)

      if (failures.nonEmpty()) {
        failures.print()
        val failedCmds = cmds.iterator.take(i + 1).toVector
        onFailure(settings, failedCmds, seed)
      }
    }

    val endTime = System.nanoTime()
    val dur     = Duration.ofNanos(endTime - startTime)
    s"$lawCount Redis laws passed ${cmds.length} tests in ${dur.conciseDesc}. (seed: $seed)"
  }

  private def onFailure(settings: Settings, failedCmds: Vector[Cmd], seed: Long)(implicit l: Line): Nothing = {
    import JsonCodecs._

    val len = failedCmds.length

    println(s"${Console.RED_B}REDIS LAW FAILURE AFTER $len COMMANDS! SEED: $seed${Console.RESET}")

    val withoutReads =
      failedCmds.filterNot(_.name.startsWith("read"))

    if (withoutReads.length < failedCmds.length) {
      val removed = failedCmds.length - withoutReads.length
      println(s"Trying again without $removed read events...")
      resetAndValidateCmds(withoutReads).unsafeRun() match {
        case Valid   => // Removing reads removed the bug, whoops. Forget this idea.
        case Invalid =>
          println(s"Success!")
          onFailure(settings, withoutReads, seed)
      }
    }

    val shrunkCmds   = settings.shrinkLimit(len).fold(failedCmds)(shrink(failedCmds, _, settings))
    val reproContent = shrunkCmds.iterator.map(_.asJson.noSpaces).mkString("[", "\n,", "\n]")
    val reproNum     = RedisLawTester.reproductionsGenerated.incrementAndGet()
    val filename     = s"/tmp/redis-bug-$reproNum.json"

    FileUtils.write(filename, reproContent)
    println(s"${Console.YELLOW_B}Wrote reproduction to $filename${Console.RESET}")
    shrunkCmds.indices.foreach(i => println(s"  ${i + 1}. ${shrunkCmds(i).name}"))
    println()

    fail(failures.failureSummary())
  }

  private def resetAndValidateCmds(newCmds: Vector[Cmd]): Fx[Validity] =
    Fx {
      val t = reset()
      t.validateCmds(newCmds)
    }

  private def shrink(cmds: Vector[Cmd], limit: Int, settings: Settings): Vector[Cmd] = {
    println(s"Shrinking ${cmds.length} commands...")

    val elementShrinker: Shrinker[Vector[Cmd]] = {
      val se1: Shrinker[VerifiedEvent.Seq] = Shrinker.maybe[VerifiedEvent.Seq](_.nonEmpty, _.tail)
      val se2: Shrinker[VerifiedEvent.Seq] = Shrinker.maybe[VerifiedEvent.Seq](_.nonEmpty, _.init)
      val se: Shrinker[VerifiedEvent.Seq] = Shrinker.combine(se1, se2)
      val ss: Shrinker[ProjectSnapshot] = Shrinker.id

      val see = se.tuple2
      val ssese = Shrinker.tuple4(ss, se, ss, se)

      val justEvictSnapshot = View(RoseTree(Cmd.EvictSnapshot))

      Shrinker.shrinkElements(Shrinker {
        case cmd @ Cmd.RunTest(test) =>

          def shrinkOption[A](o: Option[Test.Aux[A]], s: Shrinker[A]) =
            o.map { t =>
              s.shrink(t.input).map { es2 =>
                es2.map(e2 => Cmd.RunTest(t.withInput(e2)))
              }
            }

          def shrink1[A: ClassTag](s: Shrinker[A]) =
            shrinkOption(test.castAttempt[A], s)

          def shrink2[A: ClassTag, B: ClassTag](s: Shrinker[(A, B)]) =
            shrinkOption(test.castAttempt2[A, B], s)

          def shrink4[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag](s: Shrinker[(A, B, C, D)]) =
            shrinkOption(test.castAttempt4[A, B, C, D], s)

          shrink1(se)
            .orElse(shrink2(see))
            .orElse(shrink4(ssese))
            .getOrElse(View(RoseTree(cmd)))

        case Cmd.EvictSnapshot => justEvictSnapshot
      })
    }

    val shrinker: Shrinker[Vector[Cmd]] =
      Shrinker.combine(
        Shrinker.removeElements,
        elementShrinker,
      )

    val shrinkFx: Fx[Vector[Cmd]] =
      ShrinkFx(cmds)(shrinker, _.length, resetAndValidateCmds, breadthLimit = limit)

    shrinkFx.measureDuration.withTimeLimit(settings.shrinkMaxDur).unsafeRun() match {

      case Some((shrunkCmds, dur)) =>
        println(s"Shrunk ${cmds.length} commands to ${shrunkCmds.length} in ${dur.conciseDesc}.")
        shrunkCmds

      case None =>
        if (limit > 1) {
          println(s"Shrinking didn't complete in time. Trying again with breadLimit of 1...")
          shrink(cmds, 1, settings)

        } else {
          println(s"Shrinking didn't complete in time. Aborting...")
          cmds
        }
    }
  }

  def validateCmds(cmds: Vector[Cmd]): Validity = {
    for (cmd <- cmds)
      if (failures.isEmpty())
        apply(cmd)
    Invalid when failures.nonEmpty
  }

  def reset() =
    new RedisLawTester(newState)

  def loadReplay(filename: String): Vector[Cmd] = {
    val json = FileUtils.readResource(filename)
    RedisLawTester.parseCmds(json)
  }

  def replayToLast(filename: String, debug: Boolean = false)(implicit l: Line): Cmd = {
    val cmds = loadReplay(filename)
    playAll(filename, cmds.dropRight(1), cmds.length, debug)
    cmds.last
  }

  def replay(filename: String, debug: Boolean = false)(implicit l: Line): Unit = {
    val cmds = loadReplay(filename)
    playAll(filename, cmds, cmds.length, debug)
  }

  private def playAll(filename: String, cmds: Iterable[Cmd], len: Int, debug: Boolean)(implicit l: Line): Unit =
    RedisLaws.withDebugging(debug) {
      for ((cmd, i) <- cmds.iterator.zipWithIndex) {
        val prefix = s"$filename:[${i + 1}/$len]"
        if (debug)
          println(s"${Console.CYAN_B}$prefix: ${cmd.name}${Console.RESET}")
        else
          println(s"$prefix: ${cmd.name}")

        apply(cmd)

        RedisLaws.whenDebugging {
          // Test state: read
          val s1 = alg1.read(id1).unsafeRun().getOrThrow()
          val s2 = alg2.read(id2).unsafeRun().getOrThrow()
          RedisLaws.log("  -> [L] read      : ", s1)
          RedisLaws.log("  -> [R] read      : ", s2)

          // Test state: events
          val e1 = alg1.readEvents(id1, None).unsafeRun().getOrThrow()
          val e2 = alg2.readEvents(id2, None).unsafeRun().getOrThrow()
          RedisLaws.log("  -> [L] readEvents: ", e1)
          RedisLaws.log("  -> [R] readEvents: ", e2)
        }

        if (failures.nonEmpty()) {
          failures.print()
          fail(failures.failureSummary(prefix))
        }
      }
    }
}