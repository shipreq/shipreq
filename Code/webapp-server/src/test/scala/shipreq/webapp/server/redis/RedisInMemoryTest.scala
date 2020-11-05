package shipreq.webapp.server.redis

import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.server.logic.effect.Redis
import utest._

object RedisInMemoryTest extends TestSuite {

  private def tester() = {
    val id1 = ProjectId(3)
    val id2 = ProjectId(7)
    val stateFx = Fx {
      val redis = new Redis.InMemory[Fx]
      RedisLawTester.State(
        id1           = id1,
        id2           = id2,
        alg1          = redis,
        alg2          = redis,
        evictSnapshot = Fx {redis.unsafeEvictSnapshot(id1); redis.unsafeEvictSnapshot(id2)},
        publish       = redis.publishAll,
      )
    }
    RedisLawTester(stateFx)
  }

  override def tests = Tests {

    "laws" - {
      val t = tester()
      val s = RedisLawTester.Settings.default.copy(reps = 1000)
      t.testAllLaws(s)
    }

  }
}
