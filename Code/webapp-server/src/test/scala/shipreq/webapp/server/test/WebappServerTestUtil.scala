package shipreq.webapp.server.test

import cats.Eq
import japgolly.microlibs.cats_ext.CatsMacros
import java.time._
import shipreq.webapp.member.test.{WebappTestEquality, WebappTestUtil}
import shipreq.webapp.server.logic.algebra.Redis

trait WebappServerTestEquality extends WebappTestEquality {

  object ImplicitRedisEqualityDeep {
    import WebappTestUtil.ImplicitProjectEqualityDeep._
    implicit lazy val equalRedisProjectCache   : Eq[Redis.ProjectCache   ] = CatsMacros.deriveEq
    implicit lazy val equalRedisProjectSnapshot: Eq[Redis.ProjectSnapshot] = CatsMacros.deriveEq
  }

  object ImplicitRedisEqualityDeepExceptEventTime {
    import WebappTestUtil.ImplicitProjectEqualityDeepExceptEventTime._
    implicit lazy val equalRedisProjectCache   : Eq[Redis.ProjectCache   ] = CatsMacros.deriveEq
    implicit lazy val equalRedisProjectSnapshot: Eq[Redis.ProjectSnapshot] = CatsMacros.deriveEq
  }
}

// =====================================================================================================================

trait WebappServerTestUtil extends WebappTestUtil {
  import WebappServerTestUtil._

  implicit def toWSTU_IntExt(i: Int) = new WSTU_IntExt(i)
  implicit def toWSTU_DurationExt(d: Duration) = new WSTU_DurationExt(d)
}

object WebappServerTestUtil
  extends WebappServerTestEquality
     with WebappServerTestUtil {

  class WSTU_IntExt(private val i: Int) extends AnyVal {
    def second  = Duration.ofSeconds(i)
    def seconds = Duration.ofSeconds(i)
    def minute  = Duration.ofMinutes(i)
    def minutes = Duration.ofMinutes(i)
    def hour    = Duration.ofHours(i)
    def hours   = Duration.ofHours(i)
    def day     = Duration.ofDays(i)
    def days    = Duration.ofDays(i)
    def week    = Duration.ofDays(i * 7)
    def weeks   = Duration.ofDays(i * 7)
  }

  class WSTU_DurationExt(private val d: Duration) extends AnyVal {
    def ago: Instant = Instant.now().minus(d)
  }

}
