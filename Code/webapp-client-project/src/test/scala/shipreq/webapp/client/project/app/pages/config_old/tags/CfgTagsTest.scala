package shipreq.webapp.client.project.app.pages.config_old.tags

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import org.scalajs.dom.Element
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement}
import scala.annotation.tailrec
import teststate.domzipper.sizzle.Sizzle
import utest._
import shipreq.base.util.MMTree
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.UpdateConfigCmd._
import shipreq.webapp.base.test.{SampleProject => S}, S.Values._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.test._
import DataImplicits._
import MMTree.{Relations, ApplyRelations}
import TestUtil._

object CfgTagsTest extends TestSuite {

  PrepareEnv()

  @tailrec def nameCellToText(d: Element, prefix: String): String =
    d match {
      case _ if d.tagName == "INPUT" => prefix + d.asInstanceOf[HTMLInputElement].value
      case h: HTMLElement            =>
        val i = Option(h.attributes.getNamedItem("data-indent")).fold(0)(_.value.toInt)
        nameCellToText(d.firstElementChild, prefix + ("- " * i))
    }

  def nameAsTextTree(c: GenericComponent.MountedRaw) =
    Sizzle("td.name", ReactDOM.findDOMNode(c.raw).get.asElement).toVector.map(nameCellToText(_, ""))

  class FakeUpdateIO {
    var reqs = Vector.empty[(Tag, TagData)]
    val u: MainTable.DetailPaneFns.UpdateIO = (t, v, _, _) => Callback { reqs :+= ((t, v)) }
  }

  class Tester {
    lazy val filterDead = ReactTestVar[FilterDead](HideDead)
    lazy val g          = TestGlobal(S.project)
    lazy val props      = CfgTags.Props(g.sspUpdateConfig.map(_.events), g, filterDead.stateSnapshotWithReuse())
    lazy val re         = MainTable.Component(props)
    lazy val c          = ReactTestUtils.renderIntoDocument(re)
  }

  override def tests = Tests {
    val t = new Tester
    import t._

    'recvUpdates {
      import ApplicableTagGD._
      val e = Event.ApplicableTagUpdate(v10, nev(
                Name("Blah"),
                Parents(Map(1.TG -> priMed.some)),
                Children(Vector(10.TG))))
      g.applyTestEventsCB(e).runNow()

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
        rels.find(_.name == nameOfTagToClick).get.unlink.runNow()
        assertEq(t.reqs.size, 1)
        val h = t.reqs.head
        assertEq(h._1, subj)
        val actualRels = h._2.onlyThat.get
        assertEq("RFC", actualRels, expectedRels)
        val tt = ApplyRelations.trustedApply1(S.project.config.tags.tree, h._1.id, actualRels)
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
