package shipreq.taskman.server.business

import org.json4s.JsonAST.JValue
import org.specs2.mutable.Specification
import scalaz.NonEmptyList
import shipreq.base.util.ErrorOr
import shipreq.base.util.ErrorOr.Implicits._
import shipreq.base.test.specs2.BaseMatchers._
import shipreq.taskman.api.Types._
import MailingList._
import MailingList.API._
import MailChimp._
import Http._

class MailChimpTest extends Specification {

  def p[R](f: JValue => ErrorOr[R], txt: String): R =
    ErrorOr.require_!(parseIntoJson(txt) >=> f)

  "Total API failures" >> {
    "apply & unapply" in {
      val f = TotalApiFailure(1, "zz", "yy")
      ApiFailure.Total.unapply(ApiFailure.Total.apply(f)) must beSome(f)
    }

    "parseHttpErrorJson" in {
      p(parseHttpErrorJson,
        """{"status":"error","code":553,"name":"Invalid_PagingLimit","error":"Page Limit Number must be greater than or equal to 0"}"""
      ) ==== TotalApiFailure(553, "Invalid_PagingLimit", "Page Limit Number must be greater than or equal to 0")
    }
  }

  "Partial API failures" >> {
    "apply & unapply" in {
      val p1 = PartialApiFailure(1, "1!", None)
      val p2 = PartialApiFailure(2, "2!", None)
      ApiFailure.Partial.unapply(ApiFailure.Partial.apply(p1, p2 :: Nil)) must beSome(NonEmptyList(p1, p2))
    }

    "No 'errors' field" in {
      p(parsePartialFailures, """{"blah":0}""") ==== Nil
    }

    "Single with email" in {
      p(parsePartialFailures,
        """{"add_count":0,"adds":[],"update_count":0,"updates":[],"error_count":1,"errors":[{"code":250,"error":"ACCT must be provided - Value must be one of: Never, Active (not Activ)","email":{"email":"great@yay.com"}}]}"""
      ) ==== List(PartialApiFailure(250, "ACCT must be provided - Value must be one of: Never, Active (not Activ)", Some("great@yay.com".tag)))
    }

    "Single without email" in {
      p(parsePartialFailures,
        """{"add_count":0,"adds":[],"update_count":0,"updates":[],"error_count":1,"errors":[{"code":250,"error":"ACCT must be provided - Value must be one of: Never, Active (not Activ)"}]}"""
      ) ==== List(PartialApiFailure(250, "ACCT must be provided - Value must be one of: Never, Active (not Activ)", None))
    }
  }

  "lists/list" >> {
    "ok" in {
      p(parseResponse(GetListId("")),
        """{"total":1,"data":[{"id":"270dff4105","web_id":340229,"name":"Master","date_created":"2014-04-16 07:20:13","email_type_option":false,"use_awesomebar":true,"default_from_name":"Yoar Mum","default_from_email":"yoar.mum@gmail.com","default_subject":"","default_language":"en","list_rating":0,"subscribe_url_short":"http:\/\/eepurl.com\/SKedX","subscribe_url_long":"http:\/\/twitter.us8.list-manage.com\/subscribe?u=53543f1bb4e0a0dacc73d54e2&id=270dff4105","beamer_address":"us8-0b1dbef7ba-68b7f9f73e@inbound.mailchimp.com","visibility":"pub","stats":{"member_count":0,"unsubscribe_count":0,"cleaned_count":0,"member_count_since_send":0,"unsubscribe_count_since_send":0,"cleaned_count_since_send":0,"campaign_count":0,"grouping_count":0,"group_count":0,"merge_var_count":3,"avg_sub_rate":0,"avg_unsub_rate":0,"target_sub_rate":0,"open_rate":0,"click_rate":0,"date_last_campaign":null},"modules":[]}],"errors":[]}"""
      ) must beSome(ListId("270dff4105"))
    }

    "no match" in {
      p(parseResponse(GetListId("")), """{"total":0,"data":[],"errors":[]}""") must beNone
    }
  }

  "lists/batch-subscribe" >> {
    "new user" in {
      p(parseResponse(BatchSubscribe(null, null)),
        """{"add_count":1,"adds":[{"email":"great@yay.com","euid":"1fbc6c212e","leid":"147450781"}],"update_count":0,"updates":[],"error_count":0,"errors":[]}"""
      ) ==== ()
    }

    "update user" in {
      p(parseResponse(BatchSubscribe(null, null)),
        """{"add_count":0,"adds":[],"update_count":1,"updates":[{"email":"great@yay.com","euid":"1fbc6c212e","leid":"147450781"}],"error_count":0,"errors":[]}"""
      ) ==== ()
    }

    "error" in {
      parseIntoJson(
        """{"add_count":0,"adds":[],"update_count":0,"updates":[],"error_count":1,"errors":[{"code":250,"error":"ACCT must be provided - Value must be one of: Never, Active (not Activ)","email":{"email":"great@yay.com"}}]}"""
      ) >=> catchPartialFailures >=> parseResponse(BatchSubscribe(null, null)) must beAnError
    }
  }

  "lists/subscribe" >> {
    "error parsing" in {
      val f = p(parseHttpErrorJson, """{"status":"error","code":214,"name":"List_AlreadySubscribed","error":"tmp-mailchimp-app@shipreq.com is already subscribed to list Master. Click here to update your profile."}""")
      parseResponseE(Subscribe(null, null, true))(f) must beSome(AlreadySubscribed)
    }
  }
}
