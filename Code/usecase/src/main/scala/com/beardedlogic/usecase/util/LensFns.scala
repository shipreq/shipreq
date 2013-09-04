package com.beardedlogic.usecase.util

import scalaz.{LensFamily, Lens}
import Lens.{lensg, lensFamilyg}

object LensFns {

  /**
   * A lens that requires a key be provided to extract B from A.
   *
   * @tparam A The record type.
   * @tparam K The field key type.
   * @tparam B The field type.
   */
  def KeyedLens[A, K, B](set: A => K => B => A, get: A => K => B) = KeyedLens2(set, get)

  /**
   * A lens that requires a key be provided to extract B from A.
   *
   * @tparam A1 The initial record type.
   * @tparam A2 The final record type.
   * @tparam K The field key type.
   * @tparam B1 The initial field type.
   * @tparam B2 The final field type.
   */
  def KeyedLens2[A1, A2, K, B1, B2](set: A1 => K => B2 => A2, get: A1 => K => B1) =
    lensFamilyg[(A1, K), A2, B1, B2](
      ak => b => set(ak._1)(ak._2)(b),
      ak => get(ak._1)(ak._2)
    )

  private def composeKK[A1, A2, K1, B1, B2, K2, C1, C2](g: LensFamily[(A1, K1), A2, B1, B2], f: LensFamily[(B1, K2), B2, C1, C2]) =
    KeyedLens2[A1, A2, (K1, K2), C1, C2](
      a => k12 => c => {
        val (k1, k2) = k12
        val b1 = g.get((a, k1))
        val b2 = f.set((b1, k2), c)
        g.set((a, k1), b2)
      },
      a => k12 => {
        val (k1, k2) = k12
        val b = g.get(a, k1)
        f.get(b, k2)
      }
    )

  private def composeLK[A1, A2, B1, B2, K, C1, C2](g: LensFamily[A1, A2, B1, B2], f: LensFamily[(B1, K), B2, C1, C2]) =
    KeyedLens2[A1, A2, K, C1, C2](
      a => k => c => {
        val b1 = g.get(a)
        val b2 = f.set((b1, k), c)
        g.set(a, b2)
      },
      a => k => {
        val b = g.get(a)
        f.get(b, k)
      }
    )

  private def composeKL[A1, A2, K, B1, B2, C1, C2](g: LensFamily[(A1, K), A2, B1, B2], f: LensFamily[B1, B2, C1, C2]) =
    KeyedLens2[A1, A2, K, C1, C2](
      a => k => c => {
        val b1 = g.get(a, k)
        val b2 = f.set(b1, c)
        g.set((a, k), b2)
      },
      a => k => f.get(g.get(a, k))
    )

  implicit class KeyedLensExt[A1, A2, K, B1, B2](val l: LensFamily[(A1, K), A2, B1, B2]) extends AnyVal {
    def >@=@>[K2, C1, C2](f: LensFamily[(B1, K2), B2, C1, C2]) = composeKK(l, f)
    def >@=>[C1, C2](f: LensFamily[B1, B2, C1, C2]) = composeKL(l, f)
    def <=@<[Z1, Z2](g: LensFamily[Z1, Z2, A1, A2]) = composeLK(g, l)
    def <@=@<[Z1, Z2, K0](g: LensFamily[(Z1, K0), Z2, A1, A2]) = composeKK(g, l)
  }

  implicit class PlainLensExt[A1, A2, B1, B2](val l: LensFamily[A1, A2, B1, B2]) extends AnyVal {
    def >=@>[K, C1, C2](f: LensFamily[(B1, K), B2, C1, C2]) = composeLK(l, f)
    def <@=<[Z1, Z2, K](g: LensFamily[(Z1, K), Z2, A1, A2]) = composeKL(g, l)
  }

  implicit class AllLensExt[A1, A2, B](val l: LensFamily[A1, A2, B, B]) extends AnyVal {
    def <@(key: A1) = AppliedLens(l, key)
  }

}

/**
 * Composition of a lens and record instance.
 *
 * Allows stuff to be put in and taken out without specifying the source/target.
 */
class AppliedLens[A, B](val get: B, val set: B => A) {
  def mod(f: B => B) = set(f(get))
}

object AppliedLens {
  def apply[A1, A2, B](lens: LensFamily[A1, A2, B, B], key: A1): AppliedLens[A2, B] =
    new AppliedLens(lens.get(key), lens.set(key, _))
}
