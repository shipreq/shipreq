package shipreq.base.test.db

import doobie.imports._

/**
  * Created by golly on 30/12/16.
  */
object SqlTester {

  // TODO How to make this fail on exception?
  def test(q: Query[_, _]): Unit = TestDb().runNow { xa => import xa.yolo._; q.check.unsafePerformIO() }
  def test(q: Query0[_])  : Unit = TestDb().runNow { xa => import xa.yolo._; q.check.unsafePerformIO() }
  def test(q: Update[_])  : Unit = TestDb().runNow { xa => import xa.yolo._; q.check.unsafePerformIO() }
  def test(q: Update0)    : Unit = TestDb().runNow { xa => import xa.yolo._; q.check.unsafePerformIO() }

}
