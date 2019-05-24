package shipreq.webapp.server.redis

import scalaz.Name
import utest._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.server.logic.Redis

object RedisInMemoryTest extends TestSuite {

  override def tests = Tests {
    'laws {
      val id1     = ProjectId(3)
      val id2     = ProjectId(7)
      val redis   = new Redis.InMemory[Name]
      val evictSS = () => {redis.unsafeEvictSnapshot(id1); redis.unsafeEvictSnapshot(id2)}
      val await   = () => redis.publishAll.value
      val t       = new RedisLaws.Tester[Name](id1, redis, id2, redis, evictSS, await)
      // t.testAllLaws(debug = true, seed = Some(1))
      t.testAllLaws()
    }
  }
}
