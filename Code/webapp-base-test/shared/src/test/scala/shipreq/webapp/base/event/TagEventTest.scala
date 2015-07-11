package shipreq.webapp.base.event

import utest._
import shipreq.base.util.NonEmpty
import shipreq.base.util.ScalaExt._
import shipreq.base.util.UnivEq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import DeletionAction._

// =====================================================================================================================
trait TagGroupEvents {
  import TagGroupGD._

  def child(id: TagGroupId) = Children(Vector(id))
  def parent(id: TagGroupId) = Parents(Map((id: TagId) -> none))
  def ttget(tt: TagTree, ids: TagGroupId*): List[TagInTree] = ids.toList.map(i => tt.get(i).get)

  val c1Name = "Version"
  type CE = CreateTagGroup
  val c1 = CreateTagGroup(1, nev(Name(c1Name), Desc(None), MutexChildren(false)))
  val c2 = CreateTagGroup(2, nev(Name("Released"), Desc(Some("r")), MutexChildren(true), parent(1)))
  val c3 = CreateTagGroup(3, nev(Name("All"), Desc(None), MutexChildren(false), child(1)))
  val u1 = UpdateTagGroup(1, nev(Desc(Some("versionness"))))
  val List(hd1,hd2,hd3,hd4) = List(1,2,3,4).map(DeleteTagGroup(_, HardDel))
  val List(sd1,sd2,sd3,sd4) = List(1,2,3,4).map(DeleteTagGroup(_, SoftDel))
  val List( r1, r2, r3, r4) = List(1,2,3,4).map(DeleteTagGroup(_, Restore))
}

object TagGroupEventSharedTests extends SharedTests with TagGroupEvents  {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object TagGroupEventTest extends TestSuite with TagGroupEvents {
  import TagGroupGD._

  implicit class CreateTagGroupExt(private val a: CreateTagGroup) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'three - {
        val a = _assertPass(c1, c2, c3)
        val b = _assertPass(c1, c3, c2)
        assertEq(a, b)
      }
      'needName          - assertFail("Name")          (c1.mod(_ - Name))
      'needMC            - assertFail("Mutex")         (c1.mod(_ - MutexChildren))
      'badName           - assertFail("blank")         (c1.mod(_ + Name("")))
      'badDesc           - assertFail("Desc")          (c1.mod(_ + Desc(Some(tooLongStr))))
      'badChildNotFound  - assertFail("")              (c1.mod(_ + child(2)))
      'badParentNotFound - assertFail("")              (c1.mod(_ + parent(2)))
      'badChildSelf      - assertFail("")              (c1.mod(_ + child(1)))
      'badParentSelf     - assertFail("")              (c1.mod(_ + parent(1)))
      'badCycle          - assertFail("Cycle")         (c1, c2.mod(_ + child(1)))
      'dupName           - assertFail("unique")        (c1, c2.mod(_ + Name(c1Name)))
      // c/p to dead subject = bad?
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r1 = _assertPass(es: _*).config.tags.data.get(1.TG).get
        def r2 = _assertPass(es: _*).config.tags.data.get(2.TG).get
        assertEq(r1, TagInTree(TagGroup(1, c1Name, Some("versionness"), false, Live), Vector.empty))

        es :+= c2
        es :+= UpdateTagGroup(1, nev(Name("Ver"), MutexChildren(true)))
        assertEq(r1, TagInTree(TagGroup(1, "Ver", Some("versionness"), true, Live), Vector(2.TG)))
        assertEq(r2, TagInTree(TagGroup(2, "Released", Some("r"), true, Live), Vector.empty))

        // TODO confirm parent order
      }

      'badName           - assertFail("blank")    (c1, UpdateTagGroup(1, nev(Name(""))))
      'badDesc           - assertFail("Desc")     (c1, UpdateTagGroup(1, nev(Desc(Some(tooLongStr)))))
      'badChildNotFound  - assertFail("")         (c1, UpdateTagGroup(1, nev(child(2))))
      'badParentNotFound - assertFail("")         (c1, UpdateTagGroup(1, nev(parent(2))))
      'badChildSelf      - assertFail("")         (c1, UpdateTagGroup(1, nev(child(1))))
      'badParentSelf     - assertFail("")         (c1, UpdateTagGroup(1, nev(parent(1))))
      'badCycle          - assertFail("Cycle")    (c1, c2, UpdateTagGroup(1, nev(parent(2))))
      'dupName           - assertFail("unique")   (c1, c2, UpdateTagGroup(2, nev(Name(c1Name))))
      // c/p to dead subject = bad?
    }

    'delete {
      'delRest1 {
        var es = Vector[Event](c1)
        def test(e: Event, ab: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b) = ttget(t, 1, 2)
          assertEq((a, b).mapEach(_.children), (Vector(2.TG), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq(f(a, "A") + f(b, "B"), ab)
        }

        test(c2,  "AB")
        test(sd2, "A-") // softdel with live parents
        test(r2,  "AB") // restore with live parents
        test(sd2, "A-") // softdel with live parents
        test(sd1, "--") // softdel with dead children
        test(r2,  "-B") // restore with dead parents
        test(sd2, "--") // softdel with dead parents
        test(sd1, "--") // softdel with live children (sole parent)
        test(r1,  "AB") // restore with live children (sole parent)
      }

      'delRest2 {
        val c3 = CreateTagGroup(3, nev(Name("C"), Desc(None), MutexChildren(false), child(2)))
        var es = Vector[Event](c1, c2)
        def test(e: Event, acb: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b, c) = ttget(t, 1, 2, 3)
          assertEq((a, c, b).mapEach(_.children), (Vector(2.TG), Vector(2.TG), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq("[" + f(a, "A") + f(c, "C") + "]" + f(b, "B"), acb)
        }

        test(c3,  "[AC]B")
        test(sd1, "[-C]B") // softdel with live children (other live parents)
        test(sd3, "[--]-") // softdel with live children (last live parents)
        test(r1,  "[A-]-") // restore with dead children (other dead parents)
        test(r3,  "[AC]B") // restore with dead children (last dead parent)
        test(sd3, "[A-]B") // softdel with live children (other live parents)
        test(r3,  "[AC]B") // restore with live children
      }

      'delRest3 {
        val cC = CreateTagGroup(3, nev(Name("C"), Desc(None), MutexChildren(false), parent(2)))
        val cD = CreateTagGroup(4, nev(Name("D"), Desc(None), MutexChildren(false), child(2)))
        var es = Vector[Event](c1, c2, cC)
        def test(e: Event, state: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b, c, d) = ttget(t, 1, 2, 3, 4)
          assertEq((a, d, b, c).mapEach(_.children), (Vector(2.TG), Vector(2.TG), Vector(3.TG), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq("[" + f(a, "A") + f(d, "D") + "]" + f(b, "B") + f(b, "C"), state)
        }
        test(cD,  "[AD]BC")
        test(sd1, "[-D]BC")
        test(sd4, "[--]--")
        test(r4,  "[-D]--")
        test(r1,  "[AD]BC")
      }

      'delRest4 {
        val cC = CreateTagGroup(3, nev(Name("C"), Desc(None), MutexChildren(false), parent(2)))
        val cD = CreateTagGroup(4, nev(Name("D"), Desc(None), MutexChildren(false), child(3)))
        var es = Vector[Event](c1, c2, cC)
        def test(e: Event, state: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b, c, d) = ttget(t, 1, 2, 3, 4)
          assertEq((a, b, c, d).mapEach(_.children), (Vector(2.TG), Vector(3.TG), Vector.empty, Vector(3.TG)))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq("{" + f(a, "A") + f(b, "B") + "," + f(d, "D") + "}" + f(c, "C"), state)
        }
        test(cD,  "{AB,D}C")
        test(sd1, "{--,D}C")
        test(sd4, "{--,-}-")
        test(r4,  "{--,D}-")
        test(r1,  "{AB,D}C")
      }

      // TODO HardDeletion: If tag in use in [project content]     , prevent hard delete
      // TODO HardDeletion: If tag in use in [tag tree]            , it should be allowed
      // TODO HardDeletion: If tag in use in [other project config], should it should be allowed?
//      'hardTree {
//        def test(es: Event*) = assertFail("??")((c1 :: c2 :: es.toList): _*)
//        test(hd1) // live child
//        test(hd2) // live parent
//        test(hd2) // dead child
//      }
    }
  }
}

// =====================================================================================================================
trait ApplicableTagEvents {
  import ApplicableTagGD._

  def child(id: ApplicableTagId) = Children(Vector(id))
  def parent(id: ApplicableTagId) = Parents(Map((id: TagId) -> none))
  def ttget(tt: TagTree, ids: ApplicableTagId*): List[TagInTree] = ids.toList.map(i => tt.get(i).get)

  val c1Name = "Version"
  type CE = CreateApplicableTag
  val c1 = CreateApplicableTag(1, nev(Name(c1Name), Desc(None), Key("c1")))
  val c2 = CreateApplicableTag(2, nev(Name("Released"), Desc(Some("r")), Key("c2"), parent(1)))
  val c3 = CreateApplicableTag(3, nev(Name("All"), Desc(None), Key("c3"), child(1)))
  val u1 = UpdateApplicableTag(1, nev(Desc(Some("versionness"))))
  val List(hd1,hd2,hd3,hd4) = List(1,2,3,4).map(DeleteApplicableTag(_, HardDel))
  val List(sd1,sd2,sd3,sd4) = List(1,2,3,4).map(DeleteApplicableTag(_, SoftDel))
  val List( r1, r2, r3, r4) = List(1,2,3,4).map(DeleteApplicableTag(_, Restore))
}

object ApplicableTagEventSharedTests extends SharedTests with ApplicableTagEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object ApplicableTagEventTest extends TestSuite with ApplicableTagEvents {
  import ApplicableTagGD._

  implicit class CreateApplicableTagExt(private val a: CreateApplicableTag) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'three - {
        val a = _assertPass(c1, c2, c3)
        val b = _assertPass(c1, c3, c2)
        assertEq(a, b)
      }
      'needName          - assertFail("Name")          (c1.mod(_ - Name))
      'needKey           - assertFail("Key")           (c1.mod(_ - Key))
      'badName           - assertFail("blank")         (c1.mod(_ + Name("")))
      'badDesc           - assertFail("Desc")          (c1.mod(_ + Desc(Some(tooLongStr))))
      'badChildNotFound  - assertFail("")              (c1.mod(_ + child(2)))
      'badParentNotFound - assertFail("")              (c1.mod(_ + parent(2)))
      'badChildSelf      - assertFail("")              (c1.mod(_ + child(1)))
      'badParentSelf     - assertFail("")              (c1.mod(_ + parent(1)))
      'badCycle          - assertFail("Cycle")         (c1, c2.mod(_ + child(1)))
      'dupName           - assertFail("unique")        (c1, c2.mod(_ + Name(c1Name)))
      'dupKey            - assertFail("unique")        (c1, c2.mod(_ + Key("c1")))
      // c/p to dead subject = bad?
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r1 = _assertPass(es: _*).config.tags.data.get(1.AT).get
        def r2 = _assertPass(es: _*).config.tags.data.get(2.AT).get
        assertEq(r1, TagInTree(ApplicableTag(1, c1Name, Some("versionness"), "c1", Live), Vector.empty))

        es :+= c2
        es :+= UpdateApplicableTag(1, nev(Name("Ver"), Key("c=one")))
        assertEq(r1, TagInTree(ApplicableTag(1, "Ver", Some("versionness"), "c=one", Live), Vector(2.AT)))
        assertEq(r2, TagInTree(ApplicableTag(2, "Released", Some("r"), "c2", Live), Vector.empty))

        // TODO confirm parent order
      }

      'badName           - assertFail("blank")    (c1, UpdateApplicableTag(1, nev(Name(""))))
      'badDesc           - assertFail("Desc")     (c1, UpdateApplicableTag(1, nev(Desc(Some(tooLongStr)))))
      'badChildNotFound  - assertFail("")         (c1, UpdateApplicableTag(1, nev(child(2))))
      'badParentNotFound - assertFail("")         (c1, UpdateApplicableTag(1, nev(parent(2))))
      'badChildSelf      - assertFail("")         (c1, UpdateApplicableTag(1, nev(child(1))))
      'badParentSelf     - assertFail("")         (c1, UpdateApplicableTag(1, nev(parent(1))))
      'badCycle          - assertFail("Cycle")    (c1, c2, UpdateApplicableTag(1, nev(parent(2))))
      'dupName           - assertFail("unique")   (c1, c2, UpdateApplicableTag(2, nev(Name(c1Name))))
      'dupKey            - assertFail("unique")   (c1, c2, UpdateApplicableTag(2, nev(Key("c1"))))
      // c/p to dead subject = bad?
    }

    'delete {
      'delRest1 {
        var es = Vector[Event](c1)
        def test(e: Event, ab: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b) = ttget(t, 1, 2)
          assertEq((a, b).mapEach(_.children), (Vector(2.AT), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq(f(a, "A") + f(b, "B"), ab)
        }

        test(c2,  "AB")
        test(sd2, "A-") // softdel with live parents
        test(r2,  "AB") // restore with live parents
        test(sd2, "A-") // softdel with live parents
        test(sd1, "--") // softdel with dead children
        test(r2,  "-B") // restore with dead parents
        test(sd2, "--") // softdel with dead parents
        test(sd1, "--") // softdel with live children (sole parent)
        test(r1,  "AB") // restore with live children (sole parent)
      }

      'delRest2 {
        val c3 = CreateApplicableTag(3, nev(Name("C"), Desc(None), Key("c3"), child(2)))
        var es = Vector[Event](c1, c2)
        def test(e: Event, acb: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b, c) = ttget(t, 1, 2, 3)
          assertEq((a, c, b).mapEach(_.children), (Vector(2.AT), Vector(2.AT), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq("[" + f(a, "A") + f(c, "C") + "]" + f(b, "B"), acb)
        }

        test(c3,  "[AC]B")
        test(sd1, "[-C]B") // softdel with live children (other live parents)
        test(sd3, "[--]-") // softdel with live children (last live parents)
        test(r1,  "[A-]-") // restore with dead children (other dead parents)
        test(r3,  "[AC]B") // restore with dead children (last dead parent)
        test(sd3, "[A-]B") // softdel with live children (other live parents)
        test(r3,  "[AC]B") // restore with live children
      }

      'delRest3 {
        val cC = CreateApplicableTag(3, nev(Name("C"), Desc(None), Key("c"), parent(2)))
        val cD = CreateApplicableTag(4, nev(Name("D"), Desc(None), Key("d"), child(2)))
        var es = Vector[Event](c1, c2, cC)
        def test(e: Event, state: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b, c, d) = ttget(t, 1, 2, 3, 4)
          assertEq((a, d, b, c).mapEach(_.children), (Vector(2.AT), Vector(2.AT), Vector(3.AT), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq("[" + f(a, "A") + f(d, "D") + "]" + f(b, "B") + f(b, "C"), state)
        }
        test(cD,  "[AD]BC")
        test(sd1, "[-D]BC")
        test(sd4, "[--]--")
        test(r4,  "[-D]--")
        test(r1,  "[AD]BC")
      }

      'delRest4 {
        val cC = CreateApplicableTag(3, nev(Name("C"), Desc(None), Key("c"), parent(2)))
        val cD = CreateApplicableTag(4, nev(Name("D"), Desc(None), Key("d"), child(3)))
        var es = Vector[Event](c1, c2, cC)
        def test(e: Event, state: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.data
          val List(a, b, c, d) = ttget(t, 1, 2, 3, 4)
          assertEq((a, b, c, d).mapEach(_.children), (Vector(2.AT), Vector(3.AT), Vector.empty, Vector(3.AT)))
          def f(d: TagInTree, s: String) = if (d.tag.live :: Live) s else "-"
          assertEq("{" + f(a, "A") + f(b, "B") + "," + f(d, "D") + "}" + f(c, "C"), state)
        }
        test(cD,  "{AB,D}C")
        test(sd1, "{--,D}C")
        test(sd4, "{--,-}-")
        test(r4,  "{--,D}-")
        test(r1,  "{AB,D}C")
      }

      // TODO HardDeletion: If tag in use in [project content]     , prevent hard delete
      // TODO HardDeletion: If tag in use in [tag tree]            , it should be allowed
      // TODO HardDeletion: If tag in use in [other project config], should it should be allowed?
//      'hardTree {
//        def test(es: Event*) = assertFail("??")((c1 :: c2 :: es.toList): _*)
//        test(hd1) // live child
//        test(hd2) // live parent
//        test(hd2) // dead child
//      }
    }
  }
}
