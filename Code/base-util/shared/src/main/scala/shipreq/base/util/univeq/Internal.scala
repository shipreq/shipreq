package shipreq.base.util.univeq

import nyaya.util.{MultiValues, Multimap}

object Internal {
  // Copied from Shapeless
  trait =:!=[A, B]
  def _unexpected : Nothing = sys.error("Unexpected invocation")
  implicit def _neq[A, B] : A =:!= B = null.asInstanceOf[A =:!= B] //new =:!=[A, B] {}
  implicit def _neqAmbig1[A] : A =:!= A = _unexpected
  implicit def _neqAmbig2[A] : A =:!= A = _unexpected

  class UnivEqObjExt(private val self: UnivEq.type) extends AnyVal {
    @inline def emptySetMultimap[K: UnivEq, V: UnivEq] =
      Multimap.empty[K, Set, V]

    @inline def emptyMultimap[K: UnivEq, L[_] : MultiValues, V](implicit ev: L[V] =:!= Set[V]) = {
      val _ = ev
      Multimap.empty[K, L, V]
    }
  }
}
