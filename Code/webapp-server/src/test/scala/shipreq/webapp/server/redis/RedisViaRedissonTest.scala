package shipreq.webapp.server.redis

import java.time.Instant
import utest._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.server.logic.Redis
import shipreq.webapp.server.test.PrepareEnv

object RedisViaRedissonTest extends TestSuite {

  private def reps = 24

  override def tests = Tests {
    'laws {
      val client = PrepareEnv.redissonClient
      val schema = RedisSchema(s"test:${Instant.now()}:")
      val id     = ProjectId(1)
      val redis  = new RedisViaRedisson(client, schema)
      val inmem  = new Redis.InMemory[Fx]

      val evictSS = () => {
        inmem.unsafeEvictSnapshot(id)
        client.getKeys.delete(schema.snapshot(id))
        ()
      }

      val await = () => {
        inmem.publishAll.unsafeRun()
        Thread.sleep(10) // Don't remove else published messages can leak into next test
      }

      'left - {
        val t = new RedisLaws.Tester[Fx](id, redis, id, inmem, evictSS, await)
        t.testAllLaws(reps)
      }

      'right - {
        val t = new RedisLaws.Tester[Fx](id, inmem, id, redis, evictSS, await)
        t.testAllLaws(reps)
      }

    }
  }
}
