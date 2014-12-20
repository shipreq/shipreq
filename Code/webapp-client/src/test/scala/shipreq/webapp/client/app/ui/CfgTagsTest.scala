package shipreq.webapp.client.app.ui

import japgolly.scalajs.react.{TopNode, ReactComponentM_}
import japgolly.scalajs.react.test._
import org.scalajs.dom.{HTMLElement, HTMLInputElement}
import scala.annotation.tailrec
import scalaz.std.AllInstances._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.UnsafeTypes._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.{Partition, RemoteDeltaG}
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.base.protocol.Routines.TagCrud
import shipreq.webapp.client.test._
import Tag.Id
import TagProtocol._
import TestUtil._

object CfgTagsTest extends TestSuite {

  @tailrec def nameCellToText(d: Sizzle.DOM, prefix: String): String =
    d match {
      case _ if d.tagName == "INPUT"                       => prefix + d.asInstanceOf[HTMLInputElement].value
      case h: HTMLElement if h.className contains "indent" => nameCellToText(d.firstElementChild, prefix + "- ")
      case _                                               => nameCellToText(d.firstElementChild, prefix)
    }

  def nameAsTextTree(c: ReactComponentM_[TopNode]) =
    Sizzle("td.name", c.getDOMNode()).toVector.map(nameCellToText(_, ""))

  override def tests = TestSuite {
    val remote     = Routine.Remote("x", TagCrud)
    val clientData = SampleProject.clientData
    val cp         = new TestClientProtocol
    val props      = new CfgTags.Props(cp, remote, clientData, false)
    val re         = CfgTags.Component(props)
    val c          = ReactTestUtils.renderIntoDocument(re)

    'recvUpdates {
      val rev = clientData.project.tags.rev.succ
      val upd = PovTag(
        ApplicableTag(22, "Blah", None, "blah", Alive),
        PovRelations(Map(Id(1) -> Id(3).some), Vector(10)))
      val d = RemoteDeltaG(Partition.Tags, rev, rev)(Set.empty, List(upd))
      clientData.update(List(d)).unsafePerformIO()

      assertEq(nameAsTextTree(c).mkString("\n"),
        """
          |Priority
          |- High Priority
          |- Blah
          |- - Status
          |- - - WIP
          |- - - Deferred
          |- Medium Priority
          |- Low Priority
          |Version
          |- Released
          |- - v1.1
          |- v1.x
          |- - v1.1
          |- - v1.2
          |- v2.x
        """.stripMargin.trim)
    }
  }
}
