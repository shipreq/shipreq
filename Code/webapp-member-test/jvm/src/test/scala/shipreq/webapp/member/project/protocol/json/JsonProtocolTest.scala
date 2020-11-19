package shipreq.webapp.member.project.protocol.json

import nyaya.gen.Gen
import shipreq.base.test.JsonTestUtil._
import shipreq.webapp.member.project.filter.Filter.Implicits.univEqFilterValid
import shipreq.webapp.member.project.text.Text.Equality._
import shipreq.webapp.member.project.text.Text._
import shipreq.webapp.member.protocol.json.JsonCodec.Implicits._
import shipreq.webapp.member.test.project.EventEquality._
import shipreq.webapp.member.test.project.RandomData.TextGenExt
import shipreq.webapp.member.test.project.{RandomData => R, RandomEventStream}
import utest._

object JsonProtocolTest extends TestSuite {
  import Latest._
  import Latest.SavedViewCodecs._
  import Latest.AtomCodecs.instances._

  private implicit def autoSomeG[A](g: Gen[A]): Option[Gen[A]] = Some(g)

  override def tests = Tests {

    "filters" - propTestRoundTrip(R.projectConfig.flatMap(R.filter.valid.forProjectConfig))

    "savedViews" - propTestRoundTrip(R.projectNoHistory.flatMap(R.savedViews.nonEmptySavedViewsForProject))

    "text" - {
      def gr = R.reqId
      def gu = R.useCaseStepId
      def gc = R.reqCode.id
      def gi = R.customIssueTypeId
      def ga = R.applicableTagId
      "CodeGroupTitle"  - propTestRoundTrip(R.TextGen.codeGroupTitleAtom (gr, gu, gc, gi    ).text)
      "GenericReqTitle" - propTestRoundTrip(R.TextGen.genericReqTitleAtom(gr, gu, gc, gi, ga).text)
      "InlineIssueDesc" - propTestRoundTrip(R.TextGen.inlineIssueDescAtom(gr, gu, gc        ).text)
      "CustomTextField" - propTestRoundTrip(R.TextGen.customTextFieldAtom(gr, gu, gc, gi, ga).text1(CustomTextField))
      "DeletionReason"  - propTestRoundTrip(R.TextGen.deletionReasonAtom (gr, gu, gc,     ga).text1(DeletionReason))
    }

    "event" - assertRoundTrips(RandomEventStream.sampleEventStreamWithProjects.map(_._1.event))
  }
}
