package shipreq.webapp.shared.prop

class Prop[A](val test: Ctx[A] => Boolean) {

  def unary_~ = Prop.withCtx[A](x => !test(x))
  def merge(f: (Boolean, Boolean) => Boolean) = (b: Prop[A]) => Prop.withCtx[A](x => f(test(x), b.test(x)))
  def | = merge(_ || _)
  def & = merge(_ && _)
  def ==> = merge(!_ || _)
  def <==(b: Prop[A]) = b ==> this
}

object Prop {
  
  def apply[A](t: A => Boolean) = new Prop[A](x => t(x.a))
 
  def withCtx[A](t: Ctx[A] => Boolean) = new Prop[A](t)
  
}