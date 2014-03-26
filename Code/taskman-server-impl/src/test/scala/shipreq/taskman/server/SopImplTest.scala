package shipreq.taskman.server

import org.joda.time.{DateTime, Period}
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import scala.slick.jdbc.StaticQuery.query
import scala.util.Random
import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.base.util.jodatime.JodaTimeHelpers._
import shipreq.base.util.ScalaExt._
import shipreq.taskman.api.impl.Serialisation
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{MsgType, Msg, Priority}
import Msg.ReRegistrationAttempted
import Sql._

class SopImplTest extends Specification with DatabaseTest with NoTimeConversions {

  def dao = new SopImpl.Dao()
  val n = NodeId(123)
  val w = WorkerId(666)
  val rng = new Random()
  val defaultMsg = ReRegistrationAttempted("haha cool".tag)

  "getMsgsAssignNode" should {

    val insertQ = query[(Short, Priority, Priority, Option[Int], Option[Short], Period, Period, Period), MsgId](
      "INSERT INTO msgq(type, priority, priority_base, node, worker, created_at, updated_at, effective_from)" +
        "VALUES(?, ?, ?, ?, ?, now()+?, now()+?, now()+?) RETURNING id")

    def insert(node: Boolean = false,
               worker: Boolean = false,
               pri: Priority = Priority(50),
               created: Period = -1 second,
               updated: Period = null,
               effective: Period = null
                ): MsgId =
      insertQ.first(
        rng.nextInt().toShort,
        pri,
        Priority(rng.nextInt().toShort),
        if (node) Some(rng.nextInt()) else None,
        if (worker) Some(rng.nextInt().toShort) else None,
        created,
        Option(updated) getOrElse created,
        Option(effective) getOrElse created
      )

    def insertP(pris: Int*): Vector[MsgId] =
      rng.shuffle(
        pris.toVector
          .map(p => Priority(p.toShort))
          .zipWithIndex)
        .map(_.map1(p => insert(pri = p)))
        .sortBy(_._2)
        .map(_._1)

    def idAndPri(m: MsgHeader) = (m.id, m.priority)

    def returnOnly(ids: MsgId*) =
      containTheSameElementsAs(ids) ^^ {(_:List[MsgHeader]).map(_.id)}

    def returnOnlyP(idsAndPris: Seq[(MsgId, Priority)]) =
      containTheSameElementsAs(idsAndPris) ^^ {(_:List[MsgHeader]).map(idAndPri)}

    def testWithoutHighPriorityQueueOverrides(queue: Option[(Priority, Int)], allowResults: Boolean = true) = {

      def ifAllowedReturnOnly(ids: MsgId*): Matcher[List[MsgHeader]] =
        if (allowResults) returnOnly(ids: _*) else be_==(Nil)

      "return nothing when nothing is available" in {
        dao.getMsgsAssignNode(n, 2, 2 days, queue) must be empty
      }

      "return unassigned msgs" in {
        val id = insert()
        dao.getMsgsAssignNode(n, 2, 2 days, queue) must ifAllowedReturnOnly(id)
      }

      "not return recently assigned msgs" in {
        insert(created = -16 hours, updated = -15 hours, node = true)
        insert(created = -16 hours, updated = -15 hours, node = true, worker = true)
        dao.getMsgsAssignNode(n, 2, 24 hours, queue) must be empty
      }

      "ignore old assignment" in {
        val a = insert(created = -16 hours, updated = -15 hours, node = true)
        val b = insert(created = -16 hours, updated = -15 hours, node = true, worker = true)
        dao.getMsgsAssignNode(n, 2, 8 hours, queue) must ifAllowedReturnOnly(a, b)
      }

      "limit results to top n highest priority" in {
        val ids = insertP(0, 1, 2, 3, 4, 5, 6)
        dao.getMsgsAssignNode(n, 3, 8 hours, queue) must ifAllowedReturnOnly(ids takeRight 3: _*)
      }

      "filter by effective_from" in {
        insert(effective = 1 minute)
        dao.getMsgsAssignNode(n, 2, 2.days, queue) must be empty
      }
    }

    def test(limit: Int, queuedAlready: Int)(pris: Int*)(expectedIndexes: Int*) = {
      val under = List(4,5,6,7,8,9).sorted.reverse
      val allIds = insertP((under ++ pris): _*)
      val (idsU, idsP) = allIds.splitAt(under.size)
      val exps = expectedIndexes.map(i => {
        val (id, p) = if (i<0) {
                        val j = -(i+1)
                        (idsU(j), under(j))
                      } else
                        (idsP(i), pris(i))
        (id, Priority(p.toShort))
      })
      dao.getMsgsAssignNode(n, limit, 8 hours, Some((Priority(9), queuedAlready))) must returnOnlyP(exps)
    }

    "when in-memory queue is empty" >> {
      testWithoutHighPriorityQueueOverrides(None)
    }

    "when in-memory queue is full" >> {

      "ignore queue when all DB > mem" >> {
        testWithoutHighPriorityQueueOverrides(Some((Priority(0), 99)))
      }

      "ignore DB when mem > all DB" >> {
        testWithoutHighPriorityQueueOverrides(Some((Priority(200), 99)), false)
      }

      "More HP than limit" in { test(3, 99)(14, 13, 12, 11, 10)(0, 1, 2) }
      "HP equal to limit"  in { test(3, 99)(12, 11, 10        )(0, 1, 2) }
      "HP less than limit" in { test(3, 99)(11, 10            )(0, 1   ) }
    }

    "when in-memory queue is partially full" >> {
      "HP count = 0"                   in { test(5, 3)(          )(-1, -2  ) }
      "HP count = free slots"          in { test(5, 3)(11, 10    )(0, 1    ) }
      "HP count < free slots"          in { test(6, 3)(11, 10    )(0, 1, -1) }
      "HP count > free slots, < limit" in { test(5, 4)(11, 10    )(0, 1    ) }
      "HP count > free slots, = limit" in { test(3, 1)(12, 11, 10)(0, 1, 2 ) }
      "HP count > free slots, > limit" in { test(2, 1)(12, 11, 10)(0, 1    ) }
    }
  }

  "getMsgAssignWorker" should {

    val insertQ = query[(Short, Json[Msg], Option[NodeId], Option[WorkerId], Period, Period, Period), MsgId](
      "INSERT INTO msgq(type, data, node, worker, created_at, updated_at, effective_from, priority, priority_base)" +
        "VALUES(?, ?, ?, ?, now()-?, now()-?, now()-?, 5,5) RETURNING id")

    def insert(node: Option[NodeId] = None, worker: Option[WorkerId] = None, msg: Msg = defaultMsg): MsgId = {
      val p: Period = 2.days
      insertQ.first(MsgType.lookup(msg).id, Serialisation.serialise(msg), node, worker, p, p, p)
    }

    def insertAssignedToOwnNode() = insert(node = Some(n))

    def test(id: MsgId) =
      () => dao.getMsgAssignWorker(n, w, MsgHeader(id, Priority(5), new DateTime))

    "not assign if msg has been picked up by another node" in {
      test(insert(node = Some(NodeId(6789))))() must beNone
    }

    "not assign if msg has been picked up by another worker" in {
      test(insert(node = Some(n), worker = Some(WorkerId(6789))))() must beNone
    }

    "deserialise the msg" in {
      test(insertAssignedToOwnNode)() must beLike{ case Some(MsgDetail(_, msg, _)) if msg == defaultMsg => ok }
    }

    "assign when unassigned" in {
      val t = test(insertAssignedToOwnNode)
      (t().isDefined, t().isDefined) ==== (true, false)
    }
  }
}
