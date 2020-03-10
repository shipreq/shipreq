package shipreq.webapp.base.event

import io.circe.syntax._
import utest._
import shipreq.base.test.JsonTestUtil._
import shipreq.webapp.base.protocol.json.v1.Rev1._

object ProjectTemplateStabilityTest extends TestSuite {
  import EventEquality._

  protected def generate(t: ProjectTemplate): Unit = {
    val sep = "=" * 120
    val q = "\"\"\""
    val x = sep +: t.events.map(q + _.asJson.noSpaces + q + ",") :+ sep
    println(x.mkString("\n"))
  }

  override def tests = Tests {

    "V1" - {
      val pt = ProjectTemplate.V1
      val json = Seq(
        """{"CustomReqTypeCreate":{"id":1,"values":[{"imp":false},{"mnemonic":"CO"},{"name":"Constraint"}]}}""",
        """{"CustomReqTypeCreate":{"id":2,"values":[{"imp":true},{"mnemonic":"FR"},{"name":"Functional Requirement"}]}}""",
        """{"CustomReqTypeCreate":{"id":3,"values":[{"imp":false},{"mnemonic":"MF"},{"name":"Major Feature"}]}}""",
        """{"CustomReqTypeCreate":{"id":4,"values":[{"imp":false},{"mnemonic":"OE"},{"name":"Operating Environment"}]}}""",
        """{"CustomReqTypeCreate":{"id":5,"values":[{"imp":true},{"mnemonic":"QA"},{"name":"Quality Attribute"}]}}""",
        """{"CustomIssueTypeCreate":{"id":1,"values":[{"desc":"Work needs to be done."},{"key":"TODO"}]}}""",
        """{"CustomIssueTypeCreate":{"id":2,"values":[{"desc":"Waiting on external information, or an external event."},{"key":"PENDING"}]}}""",
        """{"TagGroupCreate":{"id":1,"values":[{"name":"Actors"},{"parents":{}},{"desc":null},{"children":[]},{"mutexChildren":false}]}}""",
        """{"ApplicableTagCreate":{"id":2,"values":[{"name":"Must"},{"parents":{}},{"desc":"Requirement is critical to the current delivery timebox in order for it to be a success. If even one MUST requirement is not included, the project delivery should be considered a failure"},{"children":[]},{"key":"must"}]}}""",
        """{"ApplicableTagCreate":{"id":3,"values":[{"name":"Should"},{"parents":{}},{"desc":"Requirement is important but not necessary for delivery in the current delivery timebox."},{"children":[]},{"key":"should"}]}}""",
        """{"ApplicableTagCreate":{"id":4,"values":[{"name":"Could"},{"parents":{}},{"desc":"Requirement is desirable but not necessary, and could improve user experience or customer satisfaction for little development cost. These will typically be included if time and resources permit."},{"children":[]},{"key":"could"}]}}""",
        """{"TagGroupCreate":{"id":5,"values":[{"name":"Priority"},{"parents":{}},{"desc":null},{"children":[{"a":2},{"a":3},{"a":4}]},{"mutexChildren":true}]}}""",
        """{"ApplicableTagCreate":{"id":6,"values":[{"name":"Version 1.0"},{"parents":{}},{"desc":null},{"children":[]},{"key":"v1.0"}]}}""",
        """{"TagGroupCreate":{"id":7,"values":[{"name":"Unreleased"},{"parents":{}},{"desc":"Product version in which requirements are planned for implementation."},{"children":[{"a":6}]},{"mutexChildren":false}]}}""",
        """{"TagGroupCreate":{"id":8,"values":[{"name":"Released"},{"parents":{}},{"desc":"Product version in which requirements were implemented."},{"children":[]},{"mutexChildren":false}]}}""",
        """{"TagGroupCreate":{"id":9,"values":[{"name":"Version"},{"parents":{}},{"desc":"Target product version."},{"children":[{"g":8},{"g":7}]},{"mutexChildren":false}]}}""",
        """{"FieldCustomTextCreate":{"id":1,"values":[{"key":"detail"},{"mandatory":false},{"name":"Detail"},{"reqTypes":{"all":{}}}]}}""",
        """{"FieldCustomImpCreate":{"id":2,"values":[{"mandatory":true},{"reqTypeId":{"c":3}},{"reqTypes":{"not":[{"c":4},{"c":3}]}}]}}""",
        """{"FieldCustomTagCreate":{"id":3,"values":[{"mandatory":false},{"reqTypes":{"all":{}}},{"tagId":{"g":5}}]}}""",
        """{"FieldCustomTagCreate":{"id":4,"values":[{"mandatory":false},{"reqTypes":{"all":{}}},{"tagId":{"g":9}}]}}""",
        """{"SavedViewCreate":{"id":1,"name":"Default","columns":["pubid","title","tags"],"order":{"init":[],"last":{"column":"pubid","method":"asc"}},"filterDead":"hide","filter":null}}""",
        """{"SavedViewCreate":{"id":2,"name":"By Code","columns":["code","pubid","title","tags"],"order":{"init":[{"CB":{"column":"code","method":"asc_"}}],"last":{"column":"pubid","method":"asc"}},"filterDead":"hide","filter":null}}""",
      )
      assertAllDecodeOk(json, pt.events)

    }

  }
}
