package shipreq.base.test

import scalaz.{Applicative, ~>, Name}

abstract class MockOpTransformer[Op[_], I[_]: Applicative] extends (Op ~> I) {

  var allOps = List.empty[Op[_]]

  def allOpClasses = allOps.map(_.getClass)

  def ops[T <: Op[_]](implicit m: Manifest[T]): List[T] =
    allOps.filter(s => s.getClass.isAssignableFrom(m.runtimeClass)).map(_.asInstanceOf[T])

  def sole[T <: Op[_]](implicit m: Manifest[T]): T = {
    val o = ops[T]
    if (o.size != 1) throw new AssertionError(s"Expected a single op, got: $o")
    o.head
  }

  final override def apply[A](o: Op[A]): I[A] = {
    allOps = allOps :+ o
    io(call(o))
  }

  def io[A](a: => A): I[A] = implicitly[Applicative[I]].point(a)

  def call[A]: Op[A] => A

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

case class MockOpTransformer1[Op[_], I[_]: Applicative, S <: Op[A]: Manifest, A](default: A) extends MockOpTransformer[Op, I] {
  final def SM = implicitly[Manifest[S]]

  val responses = MockResponse(default)

  override def call[A] = op =>
    if (op.getClass.isAssignableFrom(SM.runtimeClass))
      responses.pop().asInstanceOf[A]
    else
      throw new AssertionError(s"Unexpected operation: $op")

  def soleOp: S = sole[S]
}
