package shipreq.webapp.base.protocol.json.v1

import nyaya.gen.Gen
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.JsonTestUtil._
import shipreq.webapp.base.{RandomData => R}
import shipreq.webapp.base.RandomData.TextGenExt
import shipreq.webapp.base.event.EventEquality._
import shipreq.webapp.base.event.RandomEventStream
import shipreq.webapp.base.protocol.json.JsonCodec.Implicits._
import shipreq.webapp.base.text.Text._
import shipreq.webapp.base.text.Text.Equality._
import utest._

object JsonProtocolTest extends TestSuite {
  import BaseData._
  import BaseMemberData1._
  import Events._
  import AtomCodecs.instances._
  import ReqTableDataCodecs._
  import Rev1._

  private implicit def autoSomeG[A](g: Gen[A]): Option[Gen[A]] = Some(g)

  override def tests = Tests {

    'savedViews - propTestRoundTrip(R.project.flatMap(R.reqtableData.nonEmptySavedViewsForProject))

    'text - {
      def gr = R.reqId
      def gu = R.useCaseStepId
      def gc = R.reqCode.id
      def gi = R.customIssueTypeId
      def ga = R.applicableTagId
      'CodeGroupTitle  - propTestRoundTrip(R.TextGen.codeGroupTitleAtom (gr, gu, gc, gi    ).text)
      'GenericReqTitle - propTestRoundTrip(R.TextGen.genericReqTitleAtom(gr, gu, gc, gi, ga).text)
      'InlineIssueDesc - propTestRoundTrip(R.TextGen.inlineIssueDescAtom(gr, gu, gc        ).text)
      'CustomTextField - propTestRoundTrip(R.TextGen.customTextFieldAtom(gr, gu, gc, gi, ga).text1(CustomTextField))
    }

    'event - assertRoundTrips(RandomEventStream.sampleEventStreamWithProjects.map(_._1.event))
  }
}
