package shipreq.taskman.api.impl

import scala.slick.session.Session
import scalaz.~>
import shipreq.taskman.api.TaskmanApi._
import shipreq.taskman.api.Effect._

object TaskmanApiImpl {

  class GlobalContext(schema: Option[String]) {
    private[impl] val sql = new ApiSql(schema.map(_ + ".") getOrElse "")
  }

  def reify(ctx: GlobalContext, s: Session): (Cmd ~> IOM) =
    new (Cmd ~> IOM) {
      private[this] def newDao = new ApiDao(ctx, s)
      def apply[A](c: Cmd[A]): IOM[A] = c match {

        case SubmitTask1(t) => iom {
          newDao.createTask(t)
        }

        case SubmitTask(ts) => iom {
          val dao = newDao
          ts.foreach(t => dao.createTask(t))
        }

      }
    }

}
