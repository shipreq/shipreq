package shipreq.base.test

import BaseTestUtil._
import scala.reflect.ClassTag
import scalaz.{Applicative, Equal, Name, ~>}

trait OpTypeProvider[Op[_]] {
  def apply[A]: Op[A] => ClassTag[_]
}

object MockOpTransformerResults {
  def isSubtype(opType: ClassTag[_], superType: ClassTag[_]): Boolean =
    superType.runtimeClass.isAssignableFrom(opType.runtimeClass)
}

import MockOpTransformerResults.isSubtype

trait MockOpTransformerResults[Op[_]] {
  def allOps: Vector[Op[_]]

  def allOpTypes = allOps.map(o => opClassTag(o))

  def ops[T <: Op[_]](implicit T: ClassTag[T]): List[T] =
    allOps.filter(o => isSubtype(opClassTag(o), T)).toList.map(_.asInstanceOf[T])

  def sole[T <: Op[_]](implicit T: ClassTag[T]): T = {
    val o = ops[T]
    if (o.size != 1) throw new AssertionError(s"Expected a single op, got: $o")
    o.head
  }

  def opTypeProvider: OpTypeProvider[Op]
  def opClassTag[A](o: Op[A]) = opTypeProvider.apply(o).asInstanceOf[ClassTag[Op[A]]]

  def assertOpTypes(expected: ClassTag[_ <: Op[_]]*): Unit =
    assertSeq(allOpTypes, expected)(Equal.equalA, implicitly)

  private def CT[A](implicit m: ClassTag[A]) = m

  def assertOpTypes1[A <: Op[_]: ClassTag] =
    assertOpTypes(CT[A])

  def assertOpTypes2[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag] =
    assertOpTypes(CT[A], CT[B])

  def assertOpTypes3[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag] =
    assertOpTypes(CT[A], CT[B], CT[C])

  def assertOpTypes4[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag, D <: Op[_]: ClassTag] =
    assertOpTypes(CT[A], CT[B], CT[C], CT[D])

  def assertOpTypes5[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag, D <: Op[_]: ClassTag, E <: Op[_]: ClassTag] =
    assertOpTypes(CT[A], CT[B], CT[C], CT[D], CT[E])

  def assertOpTypes6[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag, D <: Op[_]: ClassTag, E <: Op[_]: ClassTag, F <: Op[_]: ClassTag] =
    assertOpTypes(CT[A], CT[B], CT[C], CT[D], CT[E], CT[F])

  def assertAnyOpsBut(notExp: ClassTag[_ <: Op[_]]*): Unit = {
    val matching = allOpTypes.filter(x => notExp.exists(y => isSubtype(x, y)))
    assert(matching.isEmpty)
  }

  def assertAnyOpsBut1[A <: Op[_]: ClassTag] = assertAnyOpsBut(CT[A])
}

abstract class MockOpTransformer[Op[_], I[_]] extends (Op ~> I) with MockOpTransformerResults[Op] {

  val x = ClassTag

  private var allOps_ = Vector.empty[Op[_]]

  override def allOps = allOps_

  final override def apply[A](o: Op[A]): I[A] = {
    allOps_ :+= o
    trans(o)
  }

  def trans[A]: Op[A] => I[A]

  case class MockResponse[A](default: A) {
    private var rs = List.empty[Name[A]]

    def <<(a: => A) = {
      rs = Name(a) :: rs
      this
    }

    def pop(): A = rs match {
      case h :: t => {rs = t; h.value}
      case Nil    => default
    }
  }
}

abstract class MockOpTransformerA[Op[_], I[_]: Applicative] extends MockOpTransformer[Op, I] {

  def io[A](a: => A): I[A] = implicitly[Applicative[I]].point(a)

  final override def trans[A] = a => io(cotrans(a))

  def cotrans[A]: Op[A] => A
}

// https://issues.scala-lang.org/browse/SI-8602
/*
case class MockOpTransformer1[Op[_], I[_]: Applicative, S <: Op[A]: ClassTag, A](ttp: OpTypeProvider[Op], default: A)
    extends MockOpTransformerA[Op, I] {

  final def S = implicitly[ClassTag[S]]

  def soleOp = sole[S]
 */
// WORKAROUND START
case class MockOpTransformer1[Op[_], I[_], S, A](ttp: OpTypeProvider[Op], default: A)(implicit I: Applicative[I], S: ClassTag[S], ev: S <:< Op[A])
  extends MockOpTransformerA[Op, I] {
  def soleOp = sole[Op[A]](S.asInstanceOf[ClassTag[Op[A]]]).asInstanceOf[S]
// WORKAROUND END

  override def opTypeProvider: OpTypeProvider[Op] = ttp

  val responses = MockResponse[A](default)

  override def cotrans[X] = op =>
    if (isSubtype(opClassTag(op), S))
      responses.pop().asInstanceOf[X]
    else
      throw new AssertionError(s"Unexpected operation: $op")
}
