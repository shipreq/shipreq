package shipreq.webapp.member.project.protocol.binary

import nyaya.gen.Gen
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.member.project.filter.Filter.Implicits.univEqFilterValid
import shipreq.webapp.member.project.text.Text._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.RandomData.TextGenExt
import shipreq.webapp.member.test.project.{RandomData => R, RandomEventStream}
import utest._

object BinaryProtocolTest extends TestSuite {
  import Latest._
  import Latest.AtomPicklers.instances._
  import Latest.SavedViewPicklers._

  private implicit def autoSomeG[A](g: Gen[A]): Option[Gen[A]] = Some(g)

  override def tests = Tests {

    "filters" - propTestRoundTripP(R.projectConfig.flatMap(R.filter.valid.forProjectConfig))

    "savedViews" - propTestRoundTripP(R.projectNoHistory.flatMap(R.savedViews.nonEmptySavedViewsForProject))

    "text" - {
      def gr = R.reqId
      def gu = R.useCaseStepId
      def gc = R.reqCode.id
      def gi = R.customIssueTypeId
      def ga = R.applicableTagId
      "CodeGroupTitle"  - propTestRoundTripP(R.TextGen.codeGroupTitleAtom (gr, gu, gc, gi    ).text)
      "GenericReqTitle" - propTestRoundTripP(R.TextGen.genericReqTitleAtom(gr, gu, gc, gi, ga).text)
      "InlineIssueDesc" - propTestRoundTripP(R.TextGen.inlineIssueDescAtom(gr, gu, gc        ).text)
      "CustomTextField" - propTestRoundTripP(R.TextGen.customTextFieldAtom(gr, gu, gc, gi, ga).text1(CustomTextField))
      "DeletionReason"  - propTestRoundTripP(R.TextGen.deletionReasonAtom (gr, gu, gc,     ga).text1(DeletionReason))
    }

    "event" - assertRoundTripsP(RandomEventStream.sampleEventStreamWithProjects.map(_._1))

    "project" - {
      import ImplicitProjectEqualityDeep._
      assertRoundTripsP(RandomEventStream.sampleEventStreamWithProjects.map(_._2))
    }

  }
}
