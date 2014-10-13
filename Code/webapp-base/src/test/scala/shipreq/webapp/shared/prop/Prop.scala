package shipreq.webapp.shared.prop

case class Prop[A](test: A => Boolean) {

  def unary_~ = Prop[A](a => !test(a))
  def |(b: Prop[A]) = Prop[A](a => test(a) || b.test(a))
  def &(b: Prop[A]) = Prop[A](a => test(a) && b.test(a))
  def ==>(b: Prop[A]) = Prop[A](a => !test(a) || b.test(a))
  def <==(b: Prop[A]) = b ==> this
}