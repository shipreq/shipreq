package shipreq.webapp.base.event

import utest._
import japgolly.microlibs.nonempty.NonEmpty
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import DataImplicits._
import Event._
import NoInitialEvents._

abstract class SharedTagEventTests extends TestSuite {
  type CE <: Event
  val c1 : CE
  val c2 : CE
  val c3 : CE
  val u1 : Event
  val sd1: Event
  val  r1: Event
  val sd2: Event
  val  r2: Event
  val sd3: Event
  val  r3: Event
  val sd4: Event
  val  r4: Event

  def addDesc  (ce: CE, d: Option[String]): CE
  def addParent(ce: CE, p: Int)           : CE
  def addChild (ce: CE, c: Int)           : CE
  def updateDesc  (subj: Int, d: Option[String]): Event
  def updateParent(subj: Int, p: Int)           : Event
  def updateChild (subj: Int, c: Int)           : Event

  def ttget(tt: TagTree, ids: Int*): List[TagInTree]
  def create(id: Int)(parents: Int*)(children: Int*): CE

  private def getChildren(t: TagInTree) =
    t.children.map(_.value)

  def tagId1: TagId
  val createTagField1 = CustomTagFieldEventTestV1.mkC1(tagId1)

  override def tests = Tests {
    'create {
      'three - {
        val a = _assertPass(c1, c2, c3)
        val b = _assertPass(c1, c3, c2)
        assertEq(a, b)
      }
      'badDesc           - assertFail("Desc")          (addDesc(c1, Some(tooLongStr)))
      'badChildNotFound  - assertFail("")              (addChild(c1, 2))
      'badParentNotFound - assertFail("")              (addParent(c1, 2))
      'badChildSelf      - assertFail("")              (addChild(c1, 1))
      'badParentSelf     - assertFail("")              (addParent(c1, 1))
      'badCycle          - assertFail("Cycle")         (c1, addChild(c2, 1))
      // child/parent to dead subject = bad?
    }

    'update {
      'badDesc           - assertFail("Desc")     (c1, updateDesc(1, Some(tooLongStr)))
      'badChildNotFound  - assertFail("")         (c1, updateChild(1, 2))
      'badParentNotFound - assertFail("")         (c1, updateParent(1, 2))
      'badChildSelf      - assertFail("")         (c1, updateChild(1, 1))
      'badParentSelf     - assertFail("")         (c1, updateParent(1, 1))
      'badCycle          - assertFail("Cycle")    (c1, c2, updateParent(1, 2))
      // child/parent to dead subject = bad?
    }

    'delete {
      'delRest1 {
        var es = Vector[Event](c1)
        def test(e: Event, ab: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.tree
          val List(a, b) = ttget(t, 1, 2)
          assertEq((a, b) mapEach getChildren, (Vector(2), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live is Live) s else "-"
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
        val c3 = create(3)()(2)
        var es = Vector[Event](c1, c2)
        def test(e: Event, acb: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.tree
          val List(a, b, c) = ttget(t, 1, 2, 3)
          assertEq((a, c, b) mapEach getChildren, (Vector(2), Vector(2), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live is Live) s else "-"
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
        val cC = create(3)(2)()
        val cD = create(4)()(2)
        var es = Vector[Event](c1, c2, cC)
        def test(e: Event, state: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.tree
          val List(a, b, c, d) = ttget(t, 1, 2, 3, 4)
          assertEq((a, d, b, c) mapEach getChildren, (Vector(2), Vector(2), Vector(3), Vector.empty))
          def f(d: TagInTree, s: String) = if (d.tag.live is Live) s else "-"
          assertEq("[" + f(a, "A") + f(d, "D") + "]" + f(b, "B") + f(b, "C"), state)
        }
        test(cD,  "[AD]BC")
        test(sd1, "[-D]BC")
        test(sd4, "[--]--")
        test(r4,  "[-D]--")
        test(r1,  "[AD]BC")
      }

      'delRest4 {
        val cC = create(3)(2)()
        val cD = create(4)()(3)
        var es = Vector[Event](c1, c2, cC)
        def test(e: Event, state: String): Unit = {
          es :+= e
          val t = _assertPass(es: _*).config.tags.tree
          val List(a, b, c, d) = ttget(t, 1, 2, 3, 4)
          assertEq((a, b, c, d) mapEach getChildren, (Vector(2), Vector(3), Vector.empty, Vector(3)))
          def f(d: TagInTree, s: String) = if (d.tag.live is Live) s else "-"
          assertEq("{" + f(a, "A") + f(b, "B") + "," + f(d, "D") + "}" + f(c, "C"), state)
        }
        test(cD,  "{AB,D}C")
        test(sd1, "{--,D}C")
        test(sd4, "{--,-}-")
        test(r4,  "{--,D}-")
        test(r1,  "{AB,D}C")
      }

      def testTagFieldLiveness(imp: Live, exp: Live)(es: Event*): Unit = {
        val p = _assertPass(es: _*)
        val f = p.config.fields.custom(createTagField1.id)
        assertEq("live", imp, f live p.config)
        assertEq("liveExplicitly", exp, f.liveExplicitly)
      }
      'whenLiveTagField {
        testTagFieldLiveness(Dead, Live)(c1, createTagField1, sd1)
        testTagFieldLiveness(Live, Live)(c1, createTagField1, sd1, r1)
      }
      'whenDeadTagField {
        testTagFieldLiveness(Dead, Dead)(c1, createTagField1, CustomTagFieldEventTestV1.sd1, sd1)
        testTagFieldLiveness(Dead, Dead)(c1, createTagField1, CustomTagFieldEventTestV1.sd1, sd1, r1)
      }
    }
  }
}


// =====================================================================================================================
trait TagGroupEvents {
  import TagGroupGD._

  def child(id: TagGroupId) = Children(Vector(id))
  def parent(id: TagGroupId) = Parents(Map((id: TagId) -> none))
  def ttget(tt: TagTree, ids: Int*): List[TagInTree] = ids.toList.map(i => tt.get(i.TG).get)
  def create(id: Int)(parents: Int*)(children: Int*) =
    TagGroupCreate(id, nev(Name(id.toString), Desc(None), Exclusivity(true),
      Children(Vector(children.map(_.TG): _*)), Parents(parents.map(_.TG -> none[TagId]).toMap)))
  def tagId1 = 1.TG

  val c1Name = "Version"
  type CE = TagGroupCreate
  val c1 = TagGroupCreate(1, nev(Name(c1Name), Desc(None), Exclusivity(false)))
  val c2 = TagGroupCreate(2, nev(Name("Released"), Desc(Some("r")), Exclusivity(true), parent(1)))
  val c3 = TagGroupCreate(3, nev(Name("All"), Desc(None), Exclusivity(false), child(1)))
  val u1 = TagGroupUpdate(1, nev(Desc(Some("versionness"))))
  val List(sd1,sd2,sd3,sd4) = List(1,2,3,4).map(i => TagDelete (i.TG))
  val List( r1, r2, r3, r4) = List(1,2,3,4).map(i => TagRestore(i.TG))

  implicit class TagGroupCreateExt(private val a: TagGroupCreate) {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  def delName  (ce: CE)                    = ce.mod(_ - Name)
  def addName  (ce: CE, n: String)         = ce.mod(_ + Name(n))
  def addDesc  (ce: CE, d: Option[String]) = ce.mod(_ + Desc(d))
  def addParent(ce: CE, p: Int)            = ce.mod(_ + parent(p))
  def addChild (ce: CE, c: Int)            = ce.mod(_ + child(c))

  def updateName  (subj: Int, n: String)         = TagGroupUpdate(subj, nev(Name(n)))
  def updateDesc  (subj: Int, d: Option[String]) = TagGroupUpdate(subj, nev(Desc(d)))
  def updateParent(subj: Int, p: Int)            = TagGroupUpdate(subj, nev(parent(p)))
  def updateChild (subj: Int, c: Int)            = TagGroupUpdate(subj, nev(child(c)))
}

object TagGroupEventSharedTests extends SharedTests with TagGroupEvents  {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object TagGroupEventSharedTests2 extends SharedTagEventTests with TagGroupEvents

object TagGroupEventTest extends TestSuite with TagGroupEvents {
  import TagGroupGD._

  override def tests = Tests {
    'create {
      'needMC - assertFail("Exclusiv")(c1.mod(_ - Exclusivity))
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r1 = _assertPass(es: _*).config.tags.tree.get(1.TG).get
        def r2 = _assertPass(es: _*).config.tags.tree.get(2.TG).get
        assertEq(r1, TagInTree(TagGroup(1, c1Name, Some("versionness"), false, Live), Vector.empty))

        es :+= c2
        es :+= TagGroupUpdate(1, nev(Name("Ver"), Exclusivity(true)))
        assertEq(r1, TagInTree(TagGroup(1, "Ver", Some("versionness"), true, Live), Vector(2.TG)))
        assertEq(r2, TagInTree(TagGroup(2, "Released", Some("r"), true, Live), Vector.empty))

        // TODO confirm parent order
      }
    }
  }
}

// =====================================================================================================================
trait ApplicableTagEvents {
  import ApplicableTagGD._

  def child(id: ApplicableTagId) = Children(Vector(id))
  def parent(id: ApplicableTagId) = Parents(Map((id: TagId) -> none))
  def ttget(tt: TagTree, ids: Int*): List[TagInTree] = ids.toList.map(i => tt.get(i.AT).get)
  def create(id: Int)(parents: Int*)(children: Int*) =
    ApplicableTagCreate(id, nev(
      Key("k" + id),
      ApplicableReqTypes(allReqTypes),
      Colour(None),
      Desc(None),
      Children(Vector(children.map(_.AT): _*)),
      Parents(parents.map(_.AT -> none[TagId]).toMap)))
  def tagId1 = 1.AT

  type CE = ApplicableTagCreate
  val c1 = ApplicableTagCreate(1, nev(Key("c1"), Colour(None), ApplicableReqTypes(allReqTypes), Desc(None)))
  val c2 = ApplicableTagCreate(2, nev(Key("c2"), Colour(None), ApplicableReqTypes(allReqTypes), Desc(Some("r")), parent(1)))
  val c3 = ApplicableTagCreate(3, nev(Key("c3"), Colour(None), ApplicableReqTypes(allReqTypes), Desc(None), child(1)))
  val u1 = ApplicableTagUpdate(1, nev(Desc(Some("versionness")), Colour(Some("#def")), ApplicableReqTypes(allReqTypes)))
  val List(sd1,sd2,sd3,sd4) = List(1,2,3,4).map(i => TagDelete (i.AT))
  val List( r1, r2, r3, r4) = List(1,2,3,4).map(i => TagRestore(i.AT))

  implicit class ApplicableTagCreateExt(private val a: ApplicableTagCreate) {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  def addDesc  (ce: CE, d: Option[String]) = ce.mod(_ + Desc(d))
  def addParent(ce: CE, p: Int)            = ce.mod(_ + parent(p))
  def addChild (ce: CE, c: Int)            = ce.mod(_ + child(c))

  def updateDesc  (subj: Int, d: Option[String]) = ApplicableTagUpdate(subj, nev(Desc(d)))
  def updateParent(subj: Int, p: Int)            = ApplicableTagUpdate(subj, nev(parent(p)))
  def updateChild (subj: Int, c: Int)            = ApplicableTagUpdate(subj, nev(child(c)))
}

object ApplicableTagEventSharedTests extends SharedTests with ApplicableTagEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object ApplicableTagEventSharedTests2 extends SharedTagEventTests with ApplicableTagEvents

object ApplicableTagEventV1Test extends TestSuite {
  import RetiredGenericData.ApplicableTagGDv1._

  def delName   (ce: CE)               = ce.mod(_ - Name)
  def addName   (ce: CE, n: String)    = ce.mod(_ + Name(n))
  def updateName(subj: Int, n: String) = ApplicableTagUpdateV1(subj, nev(Name(n)))

  def child(id: ApplicableTagId) = Children(Vector(id))
  def parent(id: ApplicableTagId) = Parents(Map((id: TagId) -> none))
  def ttget(tt: TagTree, ids: Int*): List[TagInTree] = ids.toList.map(i => tt.get(i.AT).get)
  def create(id: Int)(parents: Int*)(children: Int*) =
    ApplicableTagCreateV1(id, nev(Name(id.toString), Desc(None), Key("k" + id),
      Children(Vector(children.map(_.AT): _*)), Parents(parents.map(_.AT -> none[TagId]).toMap)))
  def tagId1 = 1.AT

  val c1Name = "Version"
  type CE = ApplicableTagCreateV1
  val c1 = ApplicableTagCreateV1(1, nev(Name(c1Name), Desc(None), Key("c1")))
  val c2 = ApplicableTagCreateV1(2, nev(Name("Released"), Desc(Some("r")), Key("c2"), parent(1)))
  val c3 = ApplicableTagCreateV1(3, nev(Name("All"), Desc(None), Key("c3"), child(1)))
  val u1 = ApplicableTagUpdateV1(1, nev(Desc(Some("versionness"))))

  implicit class ApplicableTagCreateExt(private val a: ApplicableTagCreateV1) {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {

    'create {
      'needKey  - assertFail("Key")   (c1.mod(_ - Key))
      'dupKey   - assertFail("unique")(c1, c2.mod(_ + Key("c1")))
      'needName - assertFail("Name")  (delName(c1))
      'badName  - assertFail("blank") (addName(c1, ""))
      //'dupName  - assertFail("unique")(c1, addName(c2, c1Name))
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r1 = _assertPass(es: _*).config.tags.tree.get(1.AT).get
        def r2 = _assertPass(es: _*).config.tags.tree.get(2.AT).get
        assertEq(r1, TagInTree(ApplicableTag.v1(1, c1Name, Some("versionness"), "c1", Live), Vector.empty))

        es :+= c2
        es :+= ApplicableTagUpdateV1(1, nev(Name("Ver"), Key("c=one")))
        assertEq(r1, TagInTree(ApplicableTag.v1(1, "Ver", Some("versionness"), "c=one", Live), Vector(2.AT)))
        assertEq(r2, TagInTree(ApplicableTag.v1(2, "Released", Some("r"), "c2", Live), Vector.empty))
      }

      'dupKey  - assertFail("unique")(c1, c2, ApplicableTagUpdateV1(2, nev(Key("c1"))))
      //'badName - assertFail("blank") (c1, updateName(1, ""))
      //'dupName - assertFail("unique")(c1, c2, updateName(2, c1Name))
    }
  }
}

object ApplicableTagEventTest extends TestSuite with ApplicableTagEvents {
  import ApplicableTagGD._

  override def tests = Tests {
    'create {
      'blankKey   - assertFail("Key")     (c1.mod(_ + Key("")))
      'needKey    - assertFail("Key")     (c1.mod(_ - Key))
      'dupKey     - assertFail("unique")  (c1, c2.mod(_ + Key("c1")))
      'badReqType - assertFail("ReqTypes")(c1.mod(_ + ApplicableReqTypes(onlyReqTypes(1234))))
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r1 = _assertPass(es: _*).config.tags.tree.get(1.AT).get
        def r2 = _assertPass(es: _*).config.tags.tree.get(2.AT).get
        assertEq(r1, TagInTree(ApplicableTag(1, "c1", Some("versionness"), Some("#def"), allReqTypes, Live), Vector.empty))

        es :+= CustomReqTypeEventSharedTests.c1
        es :+= c2
        es :+= ApplicableTagUpdate(1, nev(Colour(Some("#321654")), Key("c=one"), ApplicableReqTypes(onlyReqTypes(1))))
        assertEq(r1, TagInTree(ApplicableTag(1, "c=one", Some("versionness"), Some("#321654"), onlyReqTypes(1), Live), Vector(2.AT)))
        assertEq(r2, TagInTree(ApplicableTag(2, "c2", Some("r"), None, allReqTypes, Live), Vector.empty))

        // TODO confirm parent order
      }

      'blankKey   - assertFail("Key")     (c1.mod(_ + Key("")))
      'dupKey     - assertFail("unique")  (c1, c2, ApplicableTagUpdate(2, nev(Key("c1"))))
      'badReqType - assertFail("ReqTypes")(c1.mod(_ + ApplicableReqTypes(onlyReqTypes(1234))))
    }
  }
}
