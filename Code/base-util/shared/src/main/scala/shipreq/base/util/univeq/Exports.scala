package shipreq.base.util.univeq

import japgolly.univeq.{UnivEqExports, UnivEqScalaz}
import nyaya.util.Multimap

trait Exports
  extends UnivEqScalaz
     with UnivEqExports {

  @inline implicit def univEqMultimap[K, L[_], V](implicit ev: UnivEq[Map[K, L[V]]]): UnivEq[Multimap[K, L, V]] = {
    locally(ev)
    UnivEq.force
  }

  @inline implicit def UnivEqObjExt(self: UnivEq.type) =
    new Internal.UnivEqObjExt(UnivEq)
}
