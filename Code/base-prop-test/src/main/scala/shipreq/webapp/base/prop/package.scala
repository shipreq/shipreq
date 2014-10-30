package shipreq.webapp.base

import com.nicta.rng.Rng

package object prop {

  implicit class RngExt[A](val rng: Rng[A]) extends AnyVal {
    def gen = Gen.unsized(rng)
  }
}