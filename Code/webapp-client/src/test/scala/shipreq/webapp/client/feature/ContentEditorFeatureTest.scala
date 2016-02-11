package shipreq.webapp.client.feature

import monocle.Prism
import shipreq.base.test.BaseTestUtil._
import utest._
import ContentEditorFeature._
import scalaz.{Order, Equal}
import scalaz.std.option.optionEqual
import scalaz.std.map.mapEqual
import nyaya.gen._

object ContentEditorFeatureTest extends TestSuite {

  sealed trait Keys1
  case class Str1(str: String) extends Keys1
  case class Int1(int: Int)    extends Keys1

  sealed trait Keys2
  case class Str2 (str: String) extends Keys2
  case class Long2(long: Long)  extends Keys2

  sealed trait KeysN
  case class IntN (int: Int)    extends KeysN
  case class StrN (str: String) extends KeysN
  case class LongN(long: Long)  extends KeysN

  val prism1 = Prism[KeysN, Keys1] {
    case IntN (i) => Some(Int1(i))
    case StrN (s) => Some(Str1(s))
    case LongN(l) => None
  } {
    case Int1(i) => IntN(i)
    case Str1(s) => StrN(s)
  }

  val prism2 = Prism[KeysN, Keys2] {
    case LongN(i) => Some(Long2(i))
    case StrN (s) => Some(Str2(s))
    case IntN (l) => None
  } {
    case Long2(i) => LongN(i)
    case Str2 (s) => StrN(s)
  }

  implicit def uniqEvN = forceUnivEqOrderByToString[KeysN]
  implicit def uniqEv1 = forceUnivEqOrderByToString[Keys1]
  implicit def uniqEv2 = forceUnivEqOrderByToString[Keys2]
  implicit def equalEI = Equal.equalRef[EditorInstance]
  implicit def equalS1[A: Order, B: Equal] = Equal.equalBy((_: OneD.State[A, B]).values)

  case class PK[A](p: Prism[KeysN, A], k1: A)(implicit val order: Order[A], val equal: Equal[A]) {
    override def toString = k1.toString
    val kn = p reverseGet k1
  }

  def editorInstance: EditorInstance =
    new EditorInstance {
      override def toString = ##.toString
      override def render() = ???
    }

  type S2 = TwoD.State.Simple[KeysN, KeysN]
  type S1 = OneD.State.Simple[KeysN]

  val genI  = Gen.chooseInt(3)
  val genS  = Gen.choose_!(('a' to 'c').map(_.toString))
  val genL  = Gen.chooseLong(0, 3)
  val genI1 = genI map Int1
  val genS1 = genS map Str1
  val genS2 = genS map Str2
  val genL2 = genL map Long2
  val genK1 = Gen.chooseGen[Keys1](genI1, genS1)
  val genK2 = Gen.chooseGen[Keys2](genL2, genS2)
  val genK  = Gen.chooseGen[PK[_]](genK1.map(PK(prism1, _)), genK2.map(PK(prism2, _)))

  object Test1 {
    def set[A](pk: PK[A], v: => ZeroD.State)(i: S1) = {
      import pk._
      val on = i.set(kn, v)
      val o1 = i.mapK(p).set(k1, v)
      assertEq("[1] result", o1.values, on.values)
      assertEq("[1] o1 value", o1(k1), v)
      assertEq("[1] on value", on(kn), v)
      (on, o1)
    }

    def add[A](pk: PK[A]): S1 => S1 = i => {
      val e = editorInstance
      val (on, o1) = set(pk, Some(e))(i)
      assertEq(s"[1] preserve other keys on add($pk)", on.values - pk.kn, i.values - pk.kn)
      on
    }

    def clear[A](pk: PK[A]): S1 => S1 = i => {
      val (on, o1) = set(pk, None)(i)
      assertEq(s"[1] preserve other keys on clear($pk)", on.values, i.values - pk.kn)
      on
    }
  }

  object Test2 {
    def set[A, B](a: PK[A], b: PK[B], v: => ZeroD.State)(i: S2) = {
      implicit val s1Equality = equalS1(uniqEvN, b.equal)

      val (xn, x1) = Test1.set(b, v)(i(a.kn))
      val (yn, y1) = Test1.set(b, v)(i.mapK2(a.p.reverseGet)(a.k1))
      assertEq(yn, xn)
      assertEq(y1, y1)

      val on = i.set(a.kn, xn)
      val o2 = i.mapK2(a.p.reverseGet).mapK1(b.p).set(a.k1, x1)
      assertEq("[2] result", o2.values, on.values)
      assertEq("[2] o2 value", o2(a.k1), x1)
      assertEq("[2] on value", on(a.kn), xn)

      (on, o2)
    }

    def add[A, B](a: PK[A], b: PK[B]): S2 => S2 = i => {
      val e = editorInstance
      val (on, o2) = set(a, b, Some(e))(i)
      assertEq(s"[2] preserve other keys on add($a, $b)", on.values - a.kn, i.values - a.kn)
      on
    }

    def clear[A, B](a: PK[A], b: PK[B]): S2 => S2 = i => {
      val (on, o2) = set(a, b, None)(i)
      assertEq(s"[2] preserve other keys₂ on clear($a, $b)", on.values - a.kn, i.values - a.kn)

      val y = i(a.kn)
      val x = on(a.kn)
      assertEq(s"[2] preserve other keys₁ on clear($x, $y)", x.values - b.kn, y.values - b.kn)

      on
    }

    val genTest: Gen[S2 => S2] =
      for {
        action <- Gen.boolean
        a      <- genK
        b      <- genK
      } yield
        action match {
          case true  => add  (a, b)
          case false => clear(a, b)
        }
  }

  override def tests = TestSuite {
    var s: S2 = TwoD.State.init
    s.values assertEq Map.empty
    for (test <- Test2.genTest.samples().take(100)) {
      s = test(s)
      //println(s.values)
    }
  }
}
