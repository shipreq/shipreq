package shipreq.webapp.server.redis

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import nyaya.gen._
import scalaz.{Applicative, Equal, Monad, Semigroup}
import scalaz.syntax.monad._
import shipreq.base.test.SyncEffect
import shipreq.base.test.SyncEffect.Ops._
import shipreq.base.util.Util
import shipreq.webapp.base.data.{Project, ProjectId}
import shipreq.webapp.base.event._
import shipreq.webapp.base.event.EventOrd.Implicits._
import shipreq.webapp.base.hash.HashRecs
import shipreq.webapp.server.logic.Redis._
import shipreq.webapp.server.test.WebappServerTestUtil._

object RedisLaws {

  // OPS IN OWN TERMS
  // ================

  val readEvents1 = Law[O]("readEvents1")(o =>
    readEvents(o) === readEvents(None).map(_.filter(_.ord > o)))

  val readEvents2 = Law[(O, O)]("readEvents2") { case (a, b) =>
    readEvents(a) ++ readEvents(b) === readEvents(a min b) }

  val writeSnapshot2 = Law[(S, E, S, E)]("writeSnapshot2") { case (s1, e1, s2, e2) =>
    writeSnapshot(s1, e1) *> writeSnapshot(s2, e2) =-= writeSnapshot(s1 max s2, e1 ++ e2)
  }

  val writeEvents1 = Law[(E, E)]("writeEvents1") { case (c, cp) =>
    writeEvents(c, cp) === writeEvents(c -- cp, cp) }

  // This law doesn't hold because we have a property that we don't allow gaps in cached events
  // val writeEvents2 = Law[(E, E, E, E)]("writeEvents2") { case (c1, cp1, c2, cp2) =>
  //   writeEvents(c1, cp1) *> writeEvents(c2, cp2) =-= writeEvents(c1 ++ c2, cp1 ++ cp2)
  // }

  val publishEvents1 = Law[(E, E)]("publishEvents1") { case (e1, e2) =>
    publishEvents(e1) *> publishEvents(e2) === publishEvents(e1 ++ e2) }

  // OPS IN TERMS OF OTHER OPS
  // =========================

  val readEventsAsRead = Law[O]("readEventsAsRead")(o =>
    readEvents(o) === read.map(_.events.filter(_.ord > o)))

  val writeSnapshotAndPublish = Law[(S, E)]("writeSnapshotAndPublish") { case (s, e) =>
    writeSnapshot(s, e) === publishEvents(e) *> writeSnapshot(s, ∅) }

  val writeEventsAndPublish = Law[(E, E)]("writeEventsAndPublish") { case (c, cp) =>
    writeEvents(c, cp) === publishEvents(cp) *> writeEvents(c ++ cp, ∅) }

  val publishEventsViaSnapshot = Law[E]("publishEventsViaSnapshot")(e =>
    publishEvents(e) === read.flatMap(_.snapshot match {
      case Some(s) => writeSnapshot(s, e).void
      case None    => publishEvents(e)
    }))

  // R/W RELATIONSHIPS
  // =================

  val writeSnapshotAndRead = Law[(S, E)]("writeSnapshotAndRead") { case (s, e) =>
    val read2 = read.map(r => ProjectCache(r.snapshot.map(_ max s) orElse Some(s), r.events.filter(_.ord > s.ord)))
    writeSnapshot(s, e) *> read === (read2 <* writeSnapshot(s, e))
  }

  // This law doesn't hold because we have a property that we don't allow gaps in cached events
  //  val writeEventsAndRead = Law[(E, E)]("writeEventsAndRead") { case (c, cp) =>
  //    writeEvents(c, cp) *> read === (read.map(…) <* writeEvents(c, cp))
  //  }

  // ===================================================================================================================

  private def mkTestGens[F[_]: Monad](id1: ProjectId, redis1: ProjectAlgebra[F],
                                      id2: ProjectId, redis2: ProjectAlgebra[F]) = {
    var results = List.empty[Gens => Gen[Test[F]]]

    def add[I](law: Law[I])(g: Gens => Gen[I]): Unit =
      results ::= (g(_).map(law(_)(id1, redis1)(id2, redis2)))

    add(readEvents1)(_.genO)
    add(readEvents2)(_.genOO)
    add(writeSnapshot2)(_.genSESE)
    add(writeEvents1)(_.genEE)
    add(publishEvents1)(_.genEE)
    add(readEventsAsRead)(_.genO)
    add(writeSnapshotAndPublish)(_.genSE)
    add(writeEventsAndPublish)(_.genEE)
    add(publishEventsViaSnapshot)(_.genE)
    add(writeSnapshotAndRead)(_.genSE)

    results
  }

  // ===================================================================================================================

  trait Logic[A] { self =>
    def apply[G[_]: Monad]: (ProjectAlgebra[G], ProjectId) => G[A]

    def ++(fb: Logic[A])(implicit A: Semigroup[A]): Logic[A] =
      for {a <- self; b <- fb} yield A.append(a, b)

    def ===(fb: Logic[A])(implicit e: Equal[A]) =
      (this, fb, e)

    def =-=(fb: Logic[A]) =
      ===(fb)(Equal((_, _) => true))
  }

  implicit val monadLogic: Monad[Logic] = new Monad[Logic] {
    override def point[A](a: => A): Logic[A] = new Logic[A] {
      override def apply[G[_]: Monad] = (_, _) => a.point[G]
    }

    override def map[A, B](fa: Logic[A])(f: A => B): Logic[B] = new Logic[B] {
      override def apply[G[_]: Monad] = (r, id) => fa.apply[G].apply(r, id).map(f)
    }

    override def bind[A, B](fa: Logic[A])(f: A => Logic[B]): Logic[B] = new Logic[B] {
      override def apply[G[_]: Monad] = (r, id) => fa.apply[G].apply(r, id).flatMap(f(_).apply[G].apply(r, id))
    }
  }

  private implicit val eventSeqInstance: Semigroup[VerifiedEvent.Seq] =
    new Semigroup[VerifiedEvent.Seq] {
      override def append(f1: VerifiedEvent.Seq, f2: => VerifiedEvent.Seq) = f1 ++ f2
    }

  // ===================================================================================================================

  trait Law[I] { self =>
    val name: String
    type O
    val test: I => (Logic[O], Logic[O], Equal[O])

    final def apply[F[_]](i: I)
                         (id1: ProjectId, r1: ProjectAlgebra[F])
                         (id2: ProjectId, r2: ProjectAlgebra[F])
                         (implicit F: Monad[F]): Test[F] = {
      val g = test(i)
      new Test[F] {
        override val name = self.name
        override val in = i
        override type O = self.O
        override val eq = g._3
        override val lhs = g._1[F].apply(r1, id1)
        override val rhs = g._2[F].apply(r2, id2)
      }
    }
  }

  trait Test[F[_]] {
    val name: String
    val in: Any
    type O
    val eq: Equal[O]
    val lhs: F[O]
    val rhs: F[O]
  }

  object Law {
    def apply[A](name: String) = new Dsl[A](name)
    final class Dsl[A](name: String) { self =>
      def apply[B](f: A => (Logic[B], Logic[B], Equal[B])): Law[A] =
        new Law[A] {
          override val name = self.name
          override type O = B
          override val test = f
        }
    }
  }

  // ===================================================================================================================

  type E = VerifiedEvent.Seq
  type S = ProjectSnapshot
  type O = Option[EventOrd.Latest]

  private def ∅ : E = VerifiedEvent.Seq.empty

  private def publishEvents(e: E): Logic[Unit] = new Logic[Unit] {
    override def apply[G[_] : Monad] = _.publishEvents(_, e)
  }

  private def read: Logic[ProjectCache] = new Logic[ProjectCache] {
    override def apply[G[_] : Monad] = _.read(_)
  }

  private def readEvents(o: O): Logic[VerifiedEvent.Seq] = new Logic[VerifiedEvent.Seq] {
    override def apply[G[_] : Monad] = _.readEvents(_, o)
  }

  private def writeEvents(c: E, cp: E): Logic[Boolean] = new Logic[Boolean] {
    override def apply[G[_] : Monad] = _.writeEvents(_, c, cp)
  }

  private def writeSnapshot(s: S, publishOnly: E): Logic[Boolean] = new Logic[Boolean] {
    override def apply[G[_] : Monad] = _.writeSnapshot(_, s, publishOnly)
  }

  // ===================================================================================================================

  final case class Gens(genO: Gen[O],
                        genS: Gen[S],
                        genE: Gen[E]) {
    val genOO  : Gen[(O, O      )] = Gen.tuple2(genO, genO)
    val genEE  : Gen[(E, E      )] = Gen.tuple2(genE, genE)
    val genSE  : Gen[(S, E      )] = Gen.tuple2(genS, genE)
    val genSESE: Gen[(S, E, S, E)] = Gen.tuple4(genS, genE, genS, genE)
    val genEEEE: Gen[(E, E, E, E)] = Gen.tuple4(genE, genE, genE, genE)
  }

  object Gens {
    private val genZero = Gen.pure(0)

    private def chooseInt(n: Int): Gen[Int] =
      if (n > 0) Gen.chooseInt(n) else genZero

    def apply(rep: Int): Gens = {
      assert(rep <= 100, "Don't exceed 100 reps. Current formula scales too wildly")
      
      // It gives a continually good proportional increase/rep, with a reasonable total size (100 reps = 2,177)
      val limit = (Math.pow(rep + 1, 1.1) * (Math.pow(rep * 0.1, 1.1) + 1)).toInt

      val genOrd = Gen.chooseInt(limit).map(EventOrd.first + _)

      val genO: Gen[O] =
        Gen.chooseInt(limit + 1).map(i => if (i == 0) None else Some(EventOrd.Latest(i)))

      val genS: Gen[S] =
        for {
          ord <- genOrd
        } yield {
          val p = Project.empty.copy(name = ord.value.toString)
          ProjectSnapshot(p, ord.asLatest)
        }

      val genE: Gen[E] =
        for {
          start <- genOrd
          empty <- Gen.chooseInt(8)
          size  <- if (empty == 0) genZero else chooseInt(limit - start.value)
        } yield {
          def events =
            (0 until size).iterator.map { i =>
              val ord = start + i
              VerifiedEvent(ord, ProjectNameSet(ord.value.toString), HashRecs.empty)
            }
          VerifiedEvent.Seq.empty ++ events
        }

      Gens(genO, genS, genE)
    }
  }
  
  // ===================================================================================================================

  private final class Listener[F[_]](id: ProjectId, r: ProjectAlgebra[F])(implicit F: Applicative[F], S: SyncEffect[F]) {
    private val s = new collection.mutable.ArrayBuffer[VerifiedEvent]
    def get() = synchronized(s.toList)
    def clear() = synchronized(s.clear())
    private def add(e: VerifiedEvent) = F.point[Unit] { synchronized {
//      val before = get()
      s += e
//      val after = get()
//      val p =  if (r.toString.contains("Memory")) "<<M>>" else "<<R>>"
//      println(s"${p} ${before} + ${e} --> ${after}")
    }}
    r.subscribe(id, add).unsafeRun()
  }

  final class Tester[F[_]: Monad: SyncEffect](id1: ProjectId, redis1: ProjectAlgebra[F],
                                              id2: ProjectId, redis2: ProjectAlgebra[F],
                                              evictSnapshot: () => Unit,
                                              publish: () => Unit) {

    private val listener1 = new Listener(id1, redis1)
    private val listener2 = new Listener(id2, redis2)

    private implicit val retryMax = utest.asserts.RetryMax(5000.millis.asFiniteDuration)
    private implicit val retryInterval = utest.asserts.RetryInterval(5.millis.asFiniteDuration)

    def assertTest(test: Test[F]): Unit = {
      // Prepare
      listener1.clear()
      listener2.clear()

      // Run
      val o1 = test.lhs.unsafeRun()
      val o2 = test.rhs.unsafeRun()
      publish()

      // Test state
      assertState(test.name)

      // Test laws
      def p1 = listener1.get().iterator.map(_.ord).toSet
      def p2 = listener2.get().iterator.map(_.ord).toSet
      assertEq(s"${test.name} -> result", actual = o1, expect = o2)(test.eq, implicitly)
      try
        utest.eventually(p1 === p2)
      catch {
        case _: Throwable => assertEq(s"${test.name} -> published", actual = p1, expect = p2)
      }
    }

    def assertState(name: String): Unit = {
      // Test state
      val s1 = redis1.read(id1).unsafeRun()
      val s2 = redis2.read(id2).unsafeRun()
      assertEq(s"$name -> redis state", actual = s1, expect = s2)

      // Test invariants
      val s = s1
      if (s.events.size > 1) {
        val ords = s.events.iterator.map(_.ord.value).toList
        assertEq("no event gaps allowed", actual = ords, expect = Util.partitionConsecutive(ords)._1)
      }
      if (s.snapshot.isDefined && s.events.nonEmpty)
        assertEq("first event",
          actual = s.events.min.ord.value,
          expect = s.snapshot.get.ord.value + 1)

    }
    
    private val allTestGens =
      mkTestGens(id1, redis1, id2, redis2)

    private def allTestsIterator(gens            : Int => Gens,
                                 reps            : Int,
                                 seed            : Option[Long],
                                 genEvictSnapshot: Gen[Boolean]): Iterator[Test[F]] = {
      val genCtx = GenCtx(GenSize.Default, ThreadNumber(0))
      seed.foreach(genCtx.setSeed)
      val itEvictSnapshot = genEvictSnapshot.samplesUsing(genCtx)
      1.to(reps).iterator.flatMap { i =>
        if (i != 1 && itEvictSnapshot.next()) {
          evictSnapshot()
          assertState("evictSnapshot")
        }
        val gs = gens(i)
        val tests = allTestGens.map(_ (gs).samplesUsing(genCtx).next())
        Gen.shuffle(tests).samplesUsing(genCtx).next()
      }
    }

    def testAllLaws(reps : Int          = 100,
                    gens : Int => Gens  = Gens.apply,
                    seed : Option[Long] = None,
                    evict: Double       = 0.1,
                    debug: Boolean      = false): String = {
      val genEvictSS = Gen.chooseDouble(0, 1).map(_ < evict)
      val startTime  = System.nanoTime()
      val tests      = allTestsIterator(gens, reps, seed, genEvictSS).toArray
      val lawCount   = allTestGens.size
      if (debug) println(s"Testing $lawCount Redis laws, $reps times each = ${tests.length} tests")
      for (i <- tests.indices) {
        val t = tests(i)
        if (debug) printf("\n[%4d] %-24s: %s\n", i + 1, t.name, t.in.toString.take(180))
        assertTest(t)
      }
      val endTime = System.nanoTime()
      val dur     = Duration.ofNanos(endTime - startTime)
      s"$lawCount Redis laws passed after ${tests.length} tests in ${dur.conciseDesc}."
    }
  }

}