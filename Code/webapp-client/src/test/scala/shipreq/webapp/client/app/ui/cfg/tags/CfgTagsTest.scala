package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react.{TopNode, ReactComponentM_}
import japgolly.scalajs.react.test._
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement}
import scala.annotation.tailrec
import scalaz.effect.IO
import scalaz.std.AllInstances._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.UnsafeTypes._
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.{Partition, RemoteDeltaG}
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.base.protocol.Routines.TagCrud
import shipreq.webapp.base.protocol.TagProtocol._
import shipreq.webapp.base.test.{SampleProject => S}
import shipreq.webapp.base.UnsafeTypes.UnsafeIntExt
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.test._
import Tag.Id
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

  val remote = Routine.Remote("x", TagCrud)
  class Tester {
    lazy val clientData = new ClientData(S.project)
    lazy val cp         = new TestClientProtocol
    lazy val props      = new CfgTags.Props(cp, remote, clientData, false)
    lazy val re         = MainTable.Component(props)
    lazy val c          = ReactTestUtils.renderIntoDocument(re)
  }

  override def tests = TestSuite {
    val t = new Tester
    import t._

    'recvUpdates {
      val rev = clientData.project.tags.rev.succ
      val upd = PovTag(
        ApplicableTag(22, "Blah", None, "blah", Alive),
        PovRelations(Map(1.TG -> 3.AT.some), Vector(10.TG)))
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

    'detailPane {
      import DetailPane.Rels
      @inline def D = MainTable.DetailPaneFns
      val s = MainTable.initialState(props)
      val t = new FakeUpdateIO

      def testUnlink(subj: Tag, rels: Rels, nameOfTagToClick: String)(expectedRels: PovRelations): Unit = {
        rels.find(_.name == nameOfTagToClick).get.unlink.unsafePerformIO()
        assertEq(t.reqs.size, 1)
        val h = t.reqs.head
        assertEq(h._1, subj)
        val actualRels = h._2.onlyThat.get
        assertEq("RFC", actualRels, expectedRels)
        val tt = PovRelations.trustedApply1(actualRels, h._1.id, S.project.tags.data)
        assertEq("Final result", PovRelations.derive(subj.id, tt), expectedRels)
      }

      'existingParentRels {
        val subj = S.tags.get(23.AT).get.tag
        val rels = D.existingParentRels(s, t.u, subj)
        assertEq(rels.map(_.name).toList.sorted, List("Released", "v1.x"))
        // Remove parent 'Released' from 'v1.1'
        testUnlink(subj, rels, "Released")(PovRelations(Map(21.AT -> 24.AT), Vector.empty))
      }

      'existingChildRels {
        val subj = S.tags.get(27.TG).get.tag
        val rels = D.existingChildrenRels(s, t.u, subj)
        assertEq(rels.map(_.name).toList, List("v1.0", "v1.1"))
        // Remove child 'v1.0' from 'Released'
        testUnlink(subj, rels, "v1.0")(PovRelations(Map(20.TG -> 21.AT), Vector(23.AT)))
      }
    }
  }
}
