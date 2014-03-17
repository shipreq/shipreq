package shipreq.taskman.api.impl

import scala.slick.session.Session
import scalaz.~>
import shipreq.taskman.api.TaskmanApi._
import shipreq.taskman.api.Effect._

object TaskmanApiImpl {

  def reify(s: Session): (Cmd ~> IOM) =
    new (Cmd ~> IOM) {
      def apply[A](c: Cmd[A]): IOM[A] = c match {

        case SubmitTask1(t) => iom {
          new ApiDao(s).createTask(t)
        }

        case SubmitTask(ts) => iom {
          val dao = new ApiDao(s)
          ts.foreach(t => dao.createTask(t))
        }

      }
    }

}
