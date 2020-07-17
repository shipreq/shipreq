package shipreq.taskman.server.business

import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.JsonTestUtil._
import shipreq.taskman.server.business.FreshDesk._
import shipreq.taskman.server.logic.business.Support._
import utest._

object FreshDeskTest extends TestSuite {

  override def tests = Tests {

    "Get groups" - {
      def json = """[{"group":{"assign_time":null,"business_calendar_id":null,"created_at":"2014-04-30T08:05:51+09:00","description":"Product Management group","escalate_to":null,"id":1000123401,"name":"Product Management","ticket_assign_type":0,"updated_at":"2014-04-30T08:05:51+09:00","agents":[]}},{"group":{"assign_time":null,"business_calendar_id":null,"created_at":"2014-04-30T08:05:51+09:00","description":"Members of the QA team belong to this group","escalate_to":null,"id":1000123402,"name":"QA","ticket_assign_type":0,"updated_at":"2014-04-30T08:05:51+09:00","agents":[]}},{"group":{"assign_time":null,"business_calendar_id":null,"created_at":"2014-04-30T08:05:51+09:00","description":"People in the Sales team are members of this group","escalate_to":null,"id":1000123403,"name":"Sales","ticket_assign_type":0,"updated_at":"2014-04-30T08:05:51+09:00","agents":[]}}]"""

      def test(groupName: String) = {
        val groups = decodeOrThrow[List[Group]](json)
        getGroup(groups, groupName)
      }


      "find the id of a group by name" - {
        val r = test("QA")
        assert(r == \/-(Group(1000123402, "QA")))
      }

      "fail if no group returned with given name" - {
        val r = test("WHAT")
        assert(r.isLeft)
      }
    }

    "Create new ticket" - {

      def json = """{"helpdesk_ticket":{"cc_email":{"cc_emails":[],"fwd_emails":[]},"created_at":"2014-04-30T08:30:30+09:00","deleted":false,"delta":true,"description":"Details about the issue...","description_html":"Details about the issue","display_id":5,"due_by":"2014-05-03T08:30:30+09:00","email_config_id":null,"frDueBy":"2014-05-01T08:30:30+09:00","fr_escalated":false,"group_id":1000123403,"id":1004029394,"isescalated":false,"notes":[],"owner_id":null,"priority":1,"requester_id":1002397705,"responder_id":null,"source":2,"spam":false,"status":2,"subject":"Test from CURL","ticket_type":"Lead","to_email":null,"trained":false,"updated_at":"2014-04-30T08:30:30+09:00","urgent":false,"status_name":"Open","requester_status_name":"Being Processed","priority_name":"Low","source_name":"Portal","requester_name":"The Hulk","responder_name":"No Agent","to_emails":null,"custom_field":{},"attachments":[]}}"""

      "Parse success" - {
        val r = decodeOrThrow(json)(decoderTicketResponse)
        assertEq(r, TicketId(5))
      }
    }

  }
}

