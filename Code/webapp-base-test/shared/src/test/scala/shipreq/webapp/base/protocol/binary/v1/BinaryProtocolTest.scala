package shipreq.webapp.base.protocol.binary.v1

import nyaya.gen.Gen
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.RandomData.TextGenExt
import shipreq.webapp.base.event.EventEquality._
import shipreq.webapp.base.event.RandomEventStream
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text.Text._
import shipreq.webapp.base.{RandomData => R}
import utest._

object BinaryProtocolTest extends TestSuite {
  import Latest._
  import Latest.AtomPicklers.instances._
  import Latest.SavedViewPicklers._

  private implicit def autoSomeG[A](g: Gen[A]): Option[Gen[A]] = Some(g)

  override def tests = Tests {

    "savedViews" - propTestRoundTripP(R.project.flatMap(R.savedViews.nonEmptySavedViewsForProject))

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

    "project" - assertRoundTripsP(RandomEventStream.sampleEventStreamWithProjects.map(_._2))

  }
}
