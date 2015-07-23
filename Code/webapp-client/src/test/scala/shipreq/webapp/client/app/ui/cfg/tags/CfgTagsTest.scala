package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react.{TopNode, ReactComponentM_}
import japgolly.scalajs.react.test._
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement}
import scala.annotation.tailrec
import scalaz.effect.IO
import scalaz.std.AllInstances._
import utest._
import shipreq.base.util.MMTree
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.RemoteFn
import shipreq.webapp.base.protocol.RemoteFns.TagCrud
import shipreq.webapp.base.protocol.TagProtocol._
import shipreq.webapp.base.test.{SampleProject => S}, S.Values._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.HideDead
import shipreq.webapp.client.test._
import DataImplicits._
import MMTree.{Relations, ApplyRelations}
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

  class FakeUpdateIO {
    var reqs = Vector.empty[(Tag, TagCrud.V)]
    val u: MainTable.DetailPaneFns.UpdateIO = (t, v, _, _) => IO { reqs :+= ((t, v)) }
  }

  val remote = RemoteFn.Instance("x", TagCrud)
  class Tester {
    lazy val clientData = new ClientData(S.project)
    lazy val cp         = new TestClientProtocol
    lazy val props      = new CfgTags.Props(cp, remote, clientData, HideDead)
    lazy val re         = MainTable.Component(props)
    lazy val c          = ReactTestUtils.renderIntoDocument(re)
  }

  override def tests = TestSuite {
    val t = new Tester
    import t._

    'recvUpdates {
      import ApplicableTagGD._
      val e = UpdateApplicableTag(v10, nev(
                Name("Blah"),
                Parents(Map(1.TG -> priMed.some)),
                Children(Vector(10.TG))))
      val ves = verifyEvents(clientData.project)(e)
      clientData.applyEvents(ves).unsafePerformIO()

      assertEq(nameAsTextTree(c).mkString("\n"),
        """
          |Priority
          |- High Priority
          |- Blah
          |- - Status
          |- - - WIP
          |- - - Deferred
          |- - - In Production
          |- Medium Priority
          |- Low Priority
          |Version
          |- Released
          |- - v1.1
          |- v1.x
          |- - v1.1
          |- - v1.2
          |- - v1.3
          |- v2.x
        """.stripMargin.trim)
    }

    'detailPane {
      import DetailPane.Rels
      @inline def D = MainTable.DetailPaneFns
      val s = MainTable.initialState(props)
      val t = new FakeUpdateIO

      def testUnlink(subj: Tag, rels: Rels, nameOfTagToClick: String)(expectedRels: TagInTree.Relations): Unit = {
        rels.find(_.name == nameOfTagToClick).get.unlink.unsafePerformIO()
        assertEq(t.reqs.size, 1)
        val h = t.reqs.head
        assertEq(h._1, subj)
        val actualRels = h._2.onlyThat.get
        assertEq("RFC", actualRels, expectedRels)
        val tt = ApplyRelations.trustedApply1(S.project.config.tags.data, h._1.id, actualRels)
        assertEq("Final result", Relations.derive(subj.id, tt), expectedRels)
      }

      'existingParentRels {
        val subj = S.tags.get(v11).get.tag
        val rels = D.existingParentRels(s, t.u, subj)
        assertEq(rels.map(_.name).toList.sorted, List("Released", "v1.x"))
        // Remove parent 'Released' from 'v1.1'
        testUnlink(subj, rels, "Released")(Relations(Map(v1x -> v12), Vector.empty))
      }

      'existingChildRels {
        val subj = S.tags.get(27.TG).get.tag
        val rels = D.existingChildrenRels(s, t.u, subj)
        assertEq(rels.map(_.name).toList, List("v1.0", "v1.1"))
        // Remove child 'v1.0' from 'Released'
        testUnlink(subj, rels, "v1.0")(Relations(Map(20.TG -> v1x), Vector(v09, v11)))
      }
    }
  }
}
