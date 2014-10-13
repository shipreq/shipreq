package shipreq.webapp.shared.prop

class Prop[A](val test: Ctx[A] => Boolean, val name: String) {
  
  def unary_~ = Prop.withCtx[A]("¬" + name, x => !test(x))
  
  def merge(f: (Boolean, Boolean) => Boolean, g: (String, String) => String) =
    (b: Prop[A]) => Prop.withCtx[A](g(name, b.name), x => f(test(x), b.test(x)))
  
  def | = merge(_ || _, _ + " ∨ " + _)
  
  def & = merge(_ && _, _ + " ∧ " + _)
  
  def ==> = merge(!_ || _, _ + " ⇒ " + _)

  def <== = merge(_ || !_, _ + " ⇐ " + _)

  def <==> = merge(_ == _, _ + " ⇔ " + _)

  override def toString = name
}


object Prop {
  
  def apply[A](name: String, t: A => Boolean) = new Prop[A](x => t(x.a), name)
 
  def withCtx[A](name: String, t: Ctx[A] => Boolean) = new Prop[A](t, name)
  
}