package shipreq.webapp.base.protocol

import scalaz.std.vector._
import scalaz.std.string._
import scalaz.syntax.equal._
import utest._
import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.{SampleProject => S, TagId}
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import TagProtocol._
import TagTree.FlatRow
import FlatRow._
import FilterPolicy._

object TagProtocolTest extends TestSuite {

  case class TagProps(tt0: TagTree, povRels: PovRelations, t: Tag) {
    val E = EvalOver(this)
    val tt = tt0.add(TagInTree(t, Vector.empty))
    val id = tt.keys.head
    import PovRelations._

    def prop =
      DataProp.tags.tagTree(tt).liftL ==> (povRelationProps ∧ flatTreeProps)

    def povRelationProps = (
        E.equal("deriveRels(applyRels(r)) = r", derive(id, trustedApply1(povRels, id, tt)), povRels)
      ∧ E.equal("applyRels(deriveRels(t) = t",  trustedApply1(derive(id, tt), id, tt)     , tt)
      )

    def flatTreeProps = {
      def flatTree(policy: FilterPolicy) = TagTree.flatten(tt)(_.id.value % 3 == 0, policy)
      def removeBad(rows: Vector[FlatRow]) = rows.filterNot(_.status ≟ Status.Bad)
      val fno = flatTree(OmitNothing)
      val fbb = flatTree(OmitBadBranches)
      val fbp = flatTree(OmitAnythingWithBadParent)

      def sizeProps: EvalL = {
        val List(no,bb,bp) = List("no","bb","bp").map(n => s"flatTree.$n.size")
        def cmpsize(as: String, a: Int, bs: String, b: Int) = E.test(s"$as ($a) ≥ $bs ($b)", a >= b)
        s"$no ≥ $bb ≥ $bp" rename_: (cmpsize(no, fno.size, bb, fbb.size) ∧ cmpsize(bb, fbb.size, bp, fbp.size))
      }

      "TagTree flattening" rename_: (
          sizeProps
        ∧ E.equal("OmitNothing - bad = OmitBadBranches - bad", removeBad(fno), removeBad(fbb))
        )
    }
  }

  val tagPropGen: Gen[TagProps] =
    for {
      tt <- RandomData.tagTree
      t  <- RandomData.remoteDeltaG.povTag
    } yield {
      val tt2 = (tt /: t.rels.allReferencedIds)((q, id) =>
        q.modOrPut(id, identity, TagInTree(TagId.setId(t.tag, id), Vector.empty)))
      TagProps(tt2, t.rels, t.tag)
    }

  val sampleTagTree_f = {
    def t(id: Int, name: String)(children: Int*) = TagInTree(
      ApplicableTag(id, name, None, s"id=$id", if (name contains "*") Dead else Alive),
      children.toVector.map(ApplicableTag.Id(_)))
    TagTree.empty.addAll(
      t( 1, "A"  )(2, 15, 17),
      t( 2, "B*" )(3, 6, 9, 12),
      t( 3, "X*" )(4, 5),
      t( 4, "XA" )(),
      t( 5, "XB" )(),
      t( 6, "Y*" )(7, 8),
      t( 7, "YA*")(),
      t( 8, "YB*")(),
      t( 9, "Z*" )(10, 11),
      t(10, "ZA*")(),
      t(11, "ZB" )(),
      t(12, "Q*" )(13, 14),
      t(13, "QA" )(),
      t(14, "QB*")(),
      t(15, "C*" )(16),
      t(16, "CA*")(),
      t(17, "D"  )(18),
      t(18, "DA" )())
  }
  //println(TagTree.prettyPrint(sampleTagTree_f))

  override def tests = TestSuite {

    'PovRelations {
      'derive {
        // Multiple prepend parents, no children
        "22" - assertEq(PovRelations.derive(22.AT, S.tagTree), PovRelations(
          parents = Map(21.AT -> 23.AT, 27.TG -> 23.AT),
          children = Vector.empty))

        // Append parent, no children
        "24" - assertEq(PovRelations.derive(24.AT, S.tagTree), PovRelations(
          parents = Map(21.AT -> None),
          children = Vector.empty))

        // No parents, children
        "10" - assertEq(PovRelations.derive(10.TG, S.tagTree), PovRelations(
          parents = Map.empty,
          children = Vector(11.AT, 12.AT)))

        // Parents and children
        "27" - assertEq(PovRelations.derive(27.TG, S.tagTree), PovRelations(
          parents = Map(20.TG -> 21.AT),
          children = Vector(22.AT, 23.AT)))
      }
    }

    'flatTagTree {
      def show(rows: Vector[FlatRow]) = rows
        .map(r => s"${"  "*r.depth}${r.tag.name} - ${r.status} - {${r.parentPath.map(_.value).mkString(",")}}")
        .mkString("\n")

      def test(p: FilterPolicy, expect: String): Unit = {
        val a = TagTree.flatten(sampleTagTree_f)(_.alive ≟ Alive, p)
        val aa = show(a)
        assertEq(p.toString, aa, expect.trim)
      }

      'OmitNothing - test(OmitNothing,
          """
            |A - Good - {}
            |  B* - BadParentGoodKids - {1}
            |    X* - BadParentGoodKids - {1,2}
            |      XA - Good - {1,2,3}
            |      XB - Good - {1,2,3}
            |    Y* - Bad - {1,2}
            |      YA* - Bad - {1,2,6}
            |      YB* - Bad - {1,2,6}
            |    Z* - BadParentGoodKids - {1,2}
            |      ZA* - Bad - {1,2,9}
            |      ZB - Good - {1,2,9}
            |    Q* - BadParentGoodKids - {1,2}
            |      QA - Good - {1,2,12}
            |      QB* - Bad - {1,2,12}
            |  C* - Bad - {1}
            |    CA* - Bad - {1,15}
            |  D - Good - {1}
            |    DA - Good - {1,17}
          """.stripMargin)

      'OmitBadBranches - test(OmitBadBranches,
        """
          |A - Good - {}
          |  B* - BadParentGoodKids - {1}
          |    X* - BadParentGoodKids - {1,2}
          |      XA - Good - {1,2,3}
          |      XB - Good - {1,2,3}
          |    Z* - BadParentGoodKids - {1,2}
          |      ZB - Good - {1,2,9}
          |    Q* - BadParentGoodKids - {1,2}
          |      QA - Good - {1,2,12}
          |  D - Good - {1}
          |    DA - Good - {1,17}
        """.stripMargin)

      'OmitAnythingWithBadParent - test(OmitAnythingWithBadParent,
        """
          |A - Good - {}
          |  D - Good - {1}
          |    DA - Good - {1,17}
        """.stripMargin)
    }

    'props - tagPropGen.mustSatisfyE(_.prop)
  }
}