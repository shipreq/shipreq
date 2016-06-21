package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.Callback
import utest._
import shipreq.webapp.base.data.{Project, ProjectCatalogueProps}
import shipreq.webapp.base.event.RandomEventStream
import shipreq.webapp.base.test.WebappTestUtil._

/** Ensures that ClientData keeps its projectSummary() up-to-date correctly.
  */
object ClientDataTest extends TestSuite {

  override def tests = TestSuite {

    val (_, vesInit, ves) = RandomEventStream.entireEventStream(100).samples().next()

    val p = applyVerifiedEventSuccessfully(Project.empty, vesInit: _*)

    val cd = new ClientData.Impl(p, summariseProject(p, eventCount = 0))

    var processed = 0
    cd.register(c => Callback(processed += c.ves.length)).runNow()

    for (idx <- ves.indices) {
      val ve = ves(idx)

      val before = processed
      cd.applyEvents(Vector.empty :+ ve).runNow()
      assertEq("Processed events", processed, before + 1)

      val i = cd.projectSummary()

      ProjectCatalogueProps(i, cd.project(), idx + 1).assert()
    }
  }
}
