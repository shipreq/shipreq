package shipreq.webapp.server.redis

import java.time._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.server.logic.inmem.InMemoryRedis
import shipreq.webapp.server.logic.laws.RedisLawTester
import shipreq.webapp.server.test.PrepareEnv
import utest._

object RedisViaRedissonTest extends TestSuite {

  private def tester(realOnLeft: Boolean) = {
    var _nextId = Instant.now().getEpochSecond
    def nextId(): ProjectId = {
      val id = ProjectId(_nextId)
      _nextId += 1
      id
    }

    val stateFx = Fx {
      val id     = nextId()
      val client = PrepareEnv.redissonClient
      val schema = RedisSchema(s"test:${Instant.now()}:")
      val redis  = new RedisViaRedisson(client, schema)
      val inmem  = new InMemoryRedis[Fx]

      val evictSS = Fx {
        inmem.unsafeEvictSnapshot(id)
        client.getKeys.delete(schema.snapshot(id).value)
        ()
      }

      val await = Fx {
        inmem.publishAll.unsafeRun()
        Thread.sleep(10) // Don't remove else published messages can leak into next test
      }

      RedisLawTester.State(
        id1           = id,
        id2           = id,
        alg1          = if (realOnLeft) redis else inmem,
        alg2          = if (realOnLeft) inmem else redis,
        evictSnapshot = evictSS,
        publish       = await,
      )
    }

    RedisLawTester(stateFx)
  }

  override def tests = Tests {

    "laws" - {
      val s = RedisLawTester.Settings.default.withShrinkLimit(1).copy(reps = 16)
        // .copy(reps = 1000, shrinkMaxDur = Duration.ofHours(99))

      "left" - {
        val t = tester(realOnLeft = true)
        t.testAllLaws(s)
      }

      "right" - {
        val t = tester(realOnLeft = false)
        t.testAllLaws(s)
      }
    }

  }
}
