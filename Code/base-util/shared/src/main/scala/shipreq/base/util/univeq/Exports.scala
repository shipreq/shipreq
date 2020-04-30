package shipreq.base.util.univeq

import japgolly.univeq.{UnivEqExports, UnivEqScalaz}
import nyaya.util.Multimap
import scala.annotation.nowarn

trait Exports
  extends UnivEqScalaz
     with UnivEqExports {

  @nowarn("cat=unused")
  @inline implicit def univEqMultimap[K, L[_], V](implicit ev: UnivEq[Map[K, L[V]]]): UnivEq[Multimap[K, L, V]] =
    UnivEq.force

  @inline implicit def UnivEqObjExt(self: UnivEq.type) =
    new Internal.UnivEqObjExt(UnivEq)
}
