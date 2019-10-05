package shipreq.taskman.server.business

import scalaz.{\/, \/-}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.JsonTestUtil._
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.taskman.api.EmailAddr
import shipreq.taskman.server.business.MailChimp._
import shipreq.taskman.server.logic.business.MailingList._
import utest._

/*
[info] 10:15:32.814 INFO  [s.t.server.business.MailChimp   ] {          } HTTP request: POST https://us8.api.mailchimp.com/2.0/lists/list.json ← {"apikey":"<KEY>","filters":{"list_name":"Master","exact":true}}
[info] 10:15:33.894 INFO  [s.t.server.business.MailChimp   ] {          } HTTP response: 200 OK → \/-({"total":1,"data":[{"id":"270dff4105","web_id":340229,"name":"Master","date_created":"2014-04-16 07:20:13","email_type_option":false,"use_awesomebar":true,"default_from_name":"Yoar Mum","default_from_email":"yoar.mum@gmail.com","default_subject":"","default_language":"en","list_rating":0,"subscribe_url_short":"http:\/\/eepurl.com\/SKedX","subscribe_url_long":"http:\/\/twitter.us8.list-manage.com\/subscribe?u=53543f1bb4e0a0dacc73d54e2&id=270dff4105","beamer_address":"us8-0b1dbef7ba-68b7f9f73e@inbound.mailchimp.com","visibility":"pub","stats":{"member_count":8,"unsubscribe_count":0,"cleaned_count":0,"member_count_since_send":15,"unsubscribe_count_since_send":0,"cleaned_count_since_send":0,"campaign_count":0,"grouping_count":0,"group_count":0,"merge_var_count":3,"avg_sub_rate":0,"avg_unsub_rate":0,"target_sub_rate":0,"open_rate":0,"click_rate":0,"date_last_campaign":null},"modules":[]}],"errors":[]})
[info] 10:15:33.906 INFO  [s.t.server.business.MailChimp   ] {          } HTTP result: \/-(Some(ListId(270dff4105)))

[info] 10:15:33.942 INFO  [s.t.server.business.MailChimp   ] {          } HTTP request: POST https://us8.api.mailchimp.com/2.0/lists/update-member.json ← {"apikey":"<KEY>","id":"270dff4105","email":{"email":"japgolly+1503879333939@gmail.com"},"merge_vars":{"NAME":"David Barri","NEWSLETTER":1,"ACCT":"Active"}}
[info] 10:15:34.314 INFO  [s.t.server.business.MailChimp   ] {          } HTTP response: 500 Internal Server Error → -\/({"status":"error","code":232,"name":"Email_NotExists","error":"There is no record of the email address \"japgolly+1503879333939@gmail.com\" in your account"})
[info] 10:15:34.321 INFO  [s.t.server.business.MailChimp   ] {          } HTTP result: \/-(NotSubscribed)

[info] 10:15:34.322 INFO  [s.t.server.business.MailChimp   ] {          } HTTP request: POST https://us8.api.mailchimp.com/2.0/lists/subscribe.json ← {"apikey":"<KEY>","id":"270dff4105","email":{"email":"japgolly+1503879333939@gmail.com"},"merge_vars":{"NAME":"David Barri","NEWSLETTER":0,"ACCT":"Active"},"double_optin":false,"update_existing":false,"send_welcome":false}
[info] 10:15:34.943 INFO  [s.t.server.business.MailChimp   ] {          } HTTP response: 200 OK → \/-({"email":"japgolly+1503879333939@gmail.com","euid":"bea4583f78","leid":"417924229"})
[info] 10:15:34.944 INFO  [s.t.server.business.MailChimp   ] {          } HTTP result: \/-(Ok)

[info] 10:15:34.946 INFO  [s.t.server.business.MailChimp   ] {          } HTTP request: POST https://us8.api.mailchimp.com/2.0/lists/subscribe.json ← {"apikey":"<KEY>","id":"270dff4105","email":{"email":"japgolly+1503879333939@gmail.com"},"merge_vars":{"NAME":"David Barri","NEWSLETTER":0,"ACCT":"Active"},"double_optin":false,"update_existing":false,"send_welcome":false}
[info] 10:15:35.416 INFO  [s.t.server.business.MailChimp   ] {          } HTTP response: 500 Internal Server Error → -\/({"status":"error","code":214,"name":"List_AlreadySubscribed","error":"japgolly+1503879333939@gmail.com is already subscribed to the list."})
[info] 10:15:35.418 INFO  [s.t.server.business.MailChimp   ] {          } HTTP result: \/-(AlreadySubscribed)

[info] 10:15:35.420 INFO  [s.t.server.business.MailChimp   ] {          } HTTP request: POST https://us8.api.mailchimp.com/2.0/lists/update-member.json ← {"apikey":"<KEY>","id":"270dff4105","email":{"email":"japgolly+1503879333939@gmail.com"},"merge_vars":{"NAME":"David Barri","NEWSLETTER":1,"ACCT":"Active"}}
[info] 10:15:36.063 INFO  [s.t.server.business.MailChimp   ] {          } HTTP response: 200 OK → \/-({"email":"japgolly+1503879333939@gmail.com","euid":"bea4583f78","leid":"417924229"})
[info] 10:15:36.063 INFO  [s.t.server.business.MailChimp   ] {          } HTTP result: \/-(Ok)

[info] ✓ Update non-existing (NotSubscribed)
[info] ✓ Subscribe new (Ok)
[info] ✓ Subscribe existing (AlreadySubscribed)
[info] ✓ Update existing (Ok)
*/
object MailChimpTest extends TestSuite {

  override def tests = Tests {

    "Total API failures" - {

      "parseHttpErrorJson" - {
        val json = """{"status":"error","code":553,"name":"Invalid_PagingLimit","error":"Page Limit Number must be greater than or equal to 0"}"""
        val expect = ApiFailure.Total(553, "Invalid_PagingLimit", "Page Limit Number must be greater than or equal to 0")
        assertDecodeOk(json, expect)
      }
    }

    "Partial API failures" - {
      val tester = decoderTester(ApiFailure.Partial.decoderErrors)

      "No 'errors' field" - {
        tester.assertDecodeOk("""{"blah":0}""", Nil)
      }

      "Single with email" - {
        val json = """{"add_count":0,"adds":[],"update_count":0,"updates":[],"error_count":1,"errors":[{"code":250,"error":"ACCT must be provided - Value must be one of: Never, Active (not Activ)","email":{"email":"great@yay.com"}}]}"""
        val expect = List(ApiFailure.Partial(250, "ACCT must be provided - Value must be one of: Never, Active (not Activ)", Some(EmailAddr("great@yay.com"))))
        tester.assertDecodeOk(json, expect)
      }

      "Single without email" - {
        val json = """{"add_count":0,"adds":[],"update_count":0,"updates":[],"error_count":1,"errors":[{"code":250,"error":"ACCT must be provided - Value must be one of: Never, Active (not Activ)"}]}"""
        val expect = List(ApiFailure.Partial(250, "ACCT must be provided - Value must be one of: Never, Active (not Activ)", None))
        tester.assertDecodeOk(json, expect)
      }
    }

    "lists/list" - {
      val tester = decoderTester(decoderGetListIdResponse)

      "ok" - {
        val json = """{"total":1,"data":[{"id":"270dff4105","web_id":340229,"name":"Master","date_created":"2014-04-16 07:20:13","email_type_option":false,"use_awesomebar":true,"default_from_name":"Yoar Mum","default_from_email":"yoar.mum@gmail.com","default_subject":"","default_language":"en","list_rating":0,"subscribe_url_short":"http:\/\/eepurl.com\/SKedX","subscribe_url_long":"http:\/\/twitter.us8.list-manage.com\/subscribe?u=53543f1bb4e0a0dacc73d54e2&id=270dff4105","beamer_address":"us8-0b1dbef7ba-68b7f9f73e@inbound.mailchimp.com","visibility":"pub","stats":{"member_count":0,"unsubscribe_count":0,"cleaned_count":0,"member_count_since_send":0,"unsubscribe_count_since_send":0,"cleaned_count_since_send":0,"campaign_count":0,"grouping_count":0,"group_count":0,"merge_var_count":3,"avg_sub_rate":0,"avg_unsub_rate":0,"target_sub_rate":0,"open_rate":0,"click_rate":0,"date_last_campaign":null},"modules":[]}],"errors":[]}"""
        tester.assertDecodeOk(json, Some(ListId("270dff4105")))
      }

      "no match" - {
        tester.assertDecodeOk("""{"total":0,"data":[],"errors":[]}""", None)
      }
    }

    //  "lists/batch-subscribe" - {
    //    "new user" - {
    //      testParse(parseResponse(BatchSubscribe(null, null)),
    //        """{"add_count":1,"adds":[{"email":"great@yay.com","euid":"1fbc6c212e","leid":"147450781"}],"update_count":0,"updates":[],"error_count":0,"errors":[]}"""
    //      ) ==== (())
    //    }
    //
    //    "update user" - {
    //      testParse(parseResponse(BatchSubscribe(null, null)),
    //        """{"add_count":0,"adds":[],"update_count":1,"updates":[{"email":"great@yay.com","euid":"1fbc6c212e","leid":"147450781"}],"error_count":0,"errors":[]}"""
    //      ) ==== (())
    //    }
    //  }

    "lists/subscribe" - {
      "error parsing" - {
        val json = """{"status":"error","code":214,"name":"List_AlreadySubscribed","error":"tmp-mailchimp-app@shipreq.com is already subscribed to list Master. Click here to update your profile."}"""
        val result = parseErrorForSubscribe(json.toJsonOrThrow).attemptArticulateError.unsafeRun()
        assert(result == \/-(AlreadySubscribed))
      }
    }

  }
}


