package shipreq.taskman.server.app

import shipreq.taskman.server.business.MailChimp

object TmpMailchimp extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx { ctx =>
      ctx.logContent()
      val mi = ctx.mailchimp
      val io1 = mi.run(MailChimp.API.GetListId(ctx.props.mailchimp.masterList))
      val io2 = mi.run(MailChimp.API.GetListId("xx"))

      log info "Ready...."
      log info s"1) ${io1.unsafePerformIO()}"
      log info s"2) ${io2.unsafePerformIO()}"
    }

  object MailChimpTmp {
    object MasterList {

      sealed abstract class Field[V](val tag: String)
      // object EmailAddress extends Field[String]("")
      object Name extends Field[String]("NAME")
      object Newsletter extends Field[BoolAsNum]("NEWSLETTER")
      object AccountStatus extends Field[AccountStatusValue]("ACCT")

      sealed trait BoolAsNum
      object BoolAsNum {
        case object Yes extends BoolAsNum
        case object No extends BoolAsNum
      }

      sealed trait AccountStatusValue
      object AccountStatusValue {
        case object Never extends AccountStatusValue
        case object Active extends AccountStatusValue
      }
    }

    val resp = """{"total":1,"data":[{"id":"270dff4105","web_id":340229,"name":"Master","date_created":"2014-04-16 07:20:13","email_type_option":false,"use_awesomebar":true,"default_from_name":"Yoar Mum","default_from_email":"yoar.mum@gmail.com","default_subject":"","default_language":"en","list_rating":0,"subscribe_url_short":"http:\/\/eepurl.com\/SKedX","subscribe_url_long":"http:\/\/twitter.us8.list-manage.com\/subscribe?u=53543f1bb4e0a0dacc73d54e2&id=270dff4105","beamer_address":"us8-0b1dbef7ba-68b7f9f73e@inbound.mailchimp.com","visibility":"pub","stats":{"member_count":0,"unsubscribe_count":0,"cleaned_count":0,"member_count_since_send":0,"unsubscribe_count_since_send":0,"cleaned_count_since_send":0,"campaign_count":0,"grouping_count":0,"group_count":0,"merge_var_count":3,"avg_sub_rate":0,"avg_unsub_rate":0,"target_sub_rate":0,"open_rate":0,"click_rate":0,"date_last_campaign":null},"modules":[]}],"errors":[]}"""
  }
}
