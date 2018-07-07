package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.AsciiTable
import japgolly.univeq.UnivEq
import java.time.{Duration, Instant}
import monocle.macros.Lenses
import nyaya.test.Domain
import scalaz.{-\/, Equal, Name, \/, \/-}
import scalaz.syntax.functor._
import utest._
import shipreq.base.util.Retries
import shipreq.base.util.ScalaExt._
import shipreq.base.test.BaseTestUtil._
import Promise._

object PromiseTest extends TestSuite {

  type F[A] = Name[A]
  type K = Unit
  type V = Node
  type S = Node
  type A = Int

  @Lenses
  case class Node(promise: Promise[E, A])

  sealed trait E
  case object E extends E
  implicit def univEqE: UnivEq[E] = UnivEq.derive

  type InitFn = (S, E) \/ S => Option[F[E \/ A]]

  val optics: Optics[V, S, E, A] =
    Optics(monocle.std.option.some[Node].asOptional, Node.promise)

  class Tester {
    implicit val time = new MockServer
    implicit val store = Store.Algebra.concurrentHashMap[F, K, V]()

    def set(p: Option[Promise[E, A]]): Unit =
      store.storeModO(())(_ => p.map(Node.apply)).value

    def runGetOrSet(retries: Retries, initFn: InitFn): GetOrSet[E, A] =
      getOrSet(store, optics)((), retries, initFn).value.map(_._2)
  }

  implicit def initA(i: Name[A]): InitFn = _ => Some(i.map(\/-(_)))
  implicit def initE(i: Name[E]): InitFn = _ => Some(i.map(-\/(_)))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object LooseStateMachine {

    // Possible states
    sealed trait Before
    sealed trait Init
    sealed trait Retry
    sealed trait Result
    sealed trait InitRan
    case object None extends Before with Init
    case object Success extends Before with Init with Result
    case object Failure extends Before with Init with Result
    case object Working extends Before
    case object Expired extends Before
    case class Becomes(before: Before) extends Retry
    case object Timeout extends Result
    case object NoPromise extends Result
    case object InitNever extends InitRan
    case object InitOnce extends InitRan

    val Before: Domain[Before] = Domain.ofValues(None, Success, Failure, Working, Expired)
    val Retry : Domain[Retry]  = Domain.ofValues(Before.map(Becomes).iterator.toList: _*)
    val Init  : Domain[Init]   = Domain.ofValues(None, Success, Failure)

    type Inputs = (Before, Retry, Init)
    val Inputs: Domain[Inputs] = (Before *** Retry *** Init).map { case ((a, b), c) => (a, b, c) }

    /** Expectations for each combinations of states */
    val Expect: Inputs => (Result, InitRan) = {
      case (None   , _               , _      ) => (NoPromise, InitNever)
      case (Success, _               , _      ) => (Success  , InitNever)
      case (Failure, _               , None   ) => (Failure  , InitNever)
      case (Failure, _               , Failure) => (Failure  , InitOnce )
      case (Failure, _               , Success) => (Success  , InitOnce )
      case (Expired, _               , None   ) => (Timeout  , InitNever)
      case (Expired, _               , Failure) => (Failure  , InitOnce )
      case (Expired, _               , Success) => (Success  , InitOnce )
      case (Working, Becomes(None)   , _      ) => (NoPromise, InitNever)
      case (Working, Becomes(Success), _      ) => (Success  , InitNever)
      case (Working, Becomes(Failure), None   ) => (Failure  , InitNever)
      case (Working, Becomes(Failure), Success) => (Success  , InitOnce )
      case (Working, Becomes(Failure), Failure) => (Failure  , InitOnce )
      case (Working, Becomes(Working), _      ) => (Timeout  , InitNever)
      case (Working, Becomes(Expired), None   ) => (Timeout  , InitNever)
      case (Working, Becomes(Expired), _      ) => (Timeout  , InitNever) // Uses up the only retry between Working→Expired
    }

    case class TestFailure(inputs: Inputs, desc: String, actual: Any, expect: Any)

    def test(inputs: Inputs): List[TestFailure] = {
      val (before, retry, init) = inputs
      val t = new Tester; import t._

      val success = 13

      val retries = Retries(60.seconds :: Nil)
      def working = time.clock.minus(retries.totalTime).plus(100 millis)
      def expired = time.clock.minus(retries.totalTime).minus(100 millis)

      val setBefore: Before => Unit = {
        case None    => set(Option.empty)
        case Failure => set(Some(Promise.Failure(E)))
        case Success => set(Some(Promise.Available(success)))
        case Working => set(Some(Promise.InProgress(working)))
        case Expired => set(Some(Promise.InProgress(expired)))
      }

      setBefore(before)

      var actualInitCount = 0

      val initFn: InitFn = init match {
        case None    => _ => Option.empty
        case Success => Name{actualInitCount += 1; success}
        case Failure => Name{actualInitCount += 1; E}
      }

      retry match {
        case Becomes(b) => time.onDelay ::= (() => setBefore(b))
      }

      val expect = Expect(inputs)

      val expectedResult: GetOrSet[E, A] =
        expect._1 match {
          case NoPromise => GetOrSet.NoPromise
          case Timeout   => GetOrSet.Timeout
          case Failure   => GetOrSet.CustomFailure(E)
          case Success   => GetOrSet.Success(success)
        }

      val expectedInitCount: Int =
        expect._2 match {
          case InitOnce  => 1
          case InitNever => 0
        }

      var testFailures = List.empty[TestFailure]
      def testEq[A: Equal](desc: String, actual: A, expect: A): Unit =
        if (actual ≠ expect)
          testFailures ::= TestFailure(inputs, desc, actual, expect)

      testEq("Result", runGetOrSet(retries, initFn), expectedResult)
      testEq("InitRan", actualInitCount, expectedInitCount)
      testFailures
    }

    def assertTestFailures(errors: List[TestFailure]): String = {
      val total = Inputs.size
      if (errors.nonEmpty) {
        val head = Seq("Inputs", "Desc", "Actual", "Expect")
        val body = errors.map(f => Seq(f.inputs.toString, f.desc, f.actual.toString, f.expect.toString))
        val table = AsciiTable(head :: body)
        val failed = errors.map(_.inputs).toSet.size
        fail(s"${total - failed}/$total tests passed.\n$table")
      } else
        s"$total/$total tests passed"
    }

    def testAll() =
      assertTestFailures(Inputs.iterator.map(test).reduce(_ ::: _))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  override def tests = TestSuite {

    'stateMatrix {
      import LooseStateMachine._
      testAll()
      //assertTestFailures(test((Working,Becomes(Expired),Success)))
    }

  }
}
