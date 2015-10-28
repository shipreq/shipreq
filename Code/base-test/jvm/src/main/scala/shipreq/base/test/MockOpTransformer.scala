package shipreq.base.test

import scalaz.{Applicative, ~>, Name}

trait OpTypeProvider[Op[_]] {
  def apply[A]: Op[A] => Manifest[_]
}

object MockOpTransformerResults {
  def isSubtype(opType: Manifest[_], superType: Manifest[_]): Boolean = opType <:< superType
}

import MockOpTransformerResults.isSubtype

trait MockOpTransformerResults[Op[_]] {
  def allOps: Vector[Op[_]]

  def allOpTypes = allOps.map(o => opManifest(o))

  def ops[T <: Op[_]](implicit T: Manifest[T]): List[T] =
    allOps.filter(o => isSubtype(opManifest(o), T)).toList.map(_.asInstanceOf[T])

  def sole[T <: Op[_]](implicit T: Manifest[T]): T = {
    val o = ops[T]
    if (o.size != 1) throw new AssertionError(s"Expected a single op, got: $o")
    o.head
  }

  def opTypeProvider: OpTypeProvider[Op]
  def opManifest[A](o: Op[A]) = opTypeProvider.apply(o).asInstanceOf[Manifest[Op[A]]]
}

abstract class MockOpTransformer[Op[_], I[_]] extends (Op ~> I) with MockOpTransformerResults[Op] {

  val x = Manifest

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
case class MockOpTransformer1[Op[_], I[_]: Applicative, S <: Op[A]: Manifest, A](ttp: OpTypeProvider[Op], default: A)
    extends MockOpTransformerA[Op, I] {

  final def S = implicitly[Manifest[S]]

  def soleOp = sole[S]
 */
// WORKAROUND START
case class MockOpTransformer1[Op[_], I[_], S, A](ttp: OpTypeProvider[Op], default: A)(implicit I: Applicative[I], S: Manifest[S], ev: S <:< Op[A])
  extends MockOpTransformerA[Op, I] {
  def soleOp = sole[Op[A]](S.asInstanceOf[Manifest[Op[A]]]).asInstanceOf[S]
// WORKAROUND END

  override def opTypeProvider: OpTypeProvider[Op] = ttp

  val responses = MockResponse[A](default)

  override def cotrans[X] = op =>
    if (isSubtype(opManifest(op), S))
      responses.pop().asInstanceOf[X]
    else
      throw new AssertionError(s"Unexpected operation: $op")
}
