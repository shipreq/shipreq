package shipreq.webapp.base.validation2

import scalaz.{Equal, NonEmptyList}
import scalaz.syntax.equal._

object Uniqueness {

  def main[S, I, K, VS, V](key: S => K,
                           data: S => Stream[I],
                           ik: I => K,
                           vs: I => VS,
                           ignore: K => K => Boolean,
                           member: V => VS => Boolean): BF[S, V] = {
    val test = (s: S, v: V) => {
      val containsv = vs andThen member(v)
      val ignorable = ik andThen ignore(key(s))
      data(s).forall(i => !containsv(i) || ignorable(i))
    }
    new BF[S, V](ValidationPart.test(test, _))
  }

  // -------------------------------------------------------------------------------------------------------------------

  private def ignoreO[K: Equal]: Option[K] => Option[K] => Boolean =
    _.fold[Option[K] => Boolean](_ => false)(k => _.fold(false)(k ≟ _))

  def againstSetByKeyO[S, K: Equal, V](key: S => Option[K],
                                       data: S => Stream[(Option[K], Set[V])]): BF[S, V] =
    againstSetByKey[S, Option[K], V](key, data, ignoreO[K])

  def againstSetByKey[S, K, V](key: S => K,
                               data: S => Stream[(K, Set[V])],
                               ignore: K => K => Boolean): BF[S, V] =
    main[S, (K, Set[V]), K, Set[V], V](key, data, _._1, _._2, ignore, v => _ contains v)

  // -------------------------------------------------------------------------------------------------------------------

  def entity[E] = new BE[E]

  final class BE[E] {
    def apply[K: Equal, V: Equal](ek: E => K, ev: E => V): BF[(Stream[E], K), V] =
      main[(Stream[E], K), E, K, E, V](_._2, _._1, ek, identity, k => k ≟ _, v => v ≟ ev(_))
  }

  // -------------------------------------------------------------------------------------------------------------------

  def vfailure(fieldName: String): VFailure =
    VFailure.forField(fieldName, NonEmptyList("must be unique."))

  final class BF[S, V](f: VFailure => ValidationPart[S, V, V]) {
    def fieldName(fieldName: String) = f(vfailure(fieldName))
  }
}