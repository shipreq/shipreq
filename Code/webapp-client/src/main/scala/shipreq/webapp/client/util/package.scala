package shipreq.webapp.client

package object util {

  type ~=>[A, B] = ReusableFn[A, B]

  @inline implicit class AnyExtForReusable[A](val self: A) extends AnyVal {
    def ~=~(a: A)(implicit r: Reusable[A]): Boolean = r.reusable(self, a)
    def ~/~(a: A)(implicit r: Reusable[A]): Boolean = !r.reusable(self, a)
  }

}
