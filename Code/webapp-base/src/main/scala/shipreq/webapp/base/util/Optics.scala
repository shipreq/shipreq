package shipreq.webapp.base.util

import monocle._
import scalaz.Applicative
import scalaz.std.stream._
import shipreq.base.util.{UnivEq, IMap}

object Optics {

  def imapTraversal[K: UnivEq, V]: Traversal[IMap[K, V], V] = {
    type I = IMap[K, V]
    val streamTraversal = PTraversal.fromTraverse[Stream, V, V]
    new PTraversal[I, I, V, V] {
      override def modifyF[F[_] : Applicative](f: V => F[V])(i: I): F[I] = {
        val c = i.clear
        val iso = Iso[I, Stream[V]](_.values.toStream)(c ++ _)
        (iso ^|->> streamTraversal).modifyF(f)(i)
      }
    }
  }

}
