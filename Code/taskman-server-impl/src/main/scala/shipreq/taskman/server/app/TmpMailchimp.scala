package shipreq.taskman.server.app

import shipreq.taskman.server.business.MailingList._
import shipreq.taskman.server.business.MailingList.API._
import shipreq.taskman.api.Types._
import scalaz.NonEmptyList
import shipreq.base.util.ErrorOr.Implicits._

object TmpMailchimp extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx { ctx =>
      ctx.logContent()
      ctx.testConnections()

      val mi = ctx.mailchimp
      val id = ctx.mailingListId
      log info "Ready...."

      val s = Subscription("tmp-mailchimp-app@shipreq.com".tag, "Tmp MailChimp App", true, AccountStatus.Never)

      val batch = mi.run(BatchSubscribe(id, NonEmptyList(s)))
      val sub   = mi.run(Subscribe(id, s, false))
      val upd   = mi.run(UpdateMember(id, s))

      val io = batch |>==> sub |>==> upd
      io.unsafePerformIO()
    }
}
