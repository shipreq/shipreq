package shipreq.webapp.base.protocol

import utest._
import nyaya.prop._
import nyaya.gen.Gen
import nyaya.test._
import nyaya.test.PropTest._
import shipreq.base.util.MMTree
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.{SampleProject => S, TagId_T}
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import DataImplicits._
import FlatTag._
import FilterPolicy._
import MMTree.Relations
import Relations.derive

object TagCrudTest extends TestSuite {

  private def flatten(tt: TagTree)(isGood: Tag => Boolean, policy: FilterPolicy): Vector[FlatTag] =
    Tags(tt).flatRows(isGood, policy)

  case class TagProps(tt0: TagTree, povRels: TagInTree.Relations, t: Tag) {
    val E = EvalOver(this)
    val tt = tt0.add(TagInTree(t, Vector.empty))
    val id = tt.keys.head
    import MMTree.ApplyRelations._

    def prop =
      DataProp.tags.tagTree(tt).liftL ==> (povRelationProps ∧ flatTreeProps)

    def povRelationProps = (
        E.equal("deriveRels(applyRels(r)) = r", derive(id, trustedApply1(tt, id, povRels)), povRels)
      ∧ E.equal("applyRels(deriveRels(t) = t",  trustedApply1(tt, id, derive(id, tt))     , tt)
      )

    def flatTreeProps = {
      def flatTree(policy: FilterPolicy) = flatten(tt)(_.id.value % 3 == 0, policy)
      def removeBad(rows: Vector[FlatTag]) = rows.filterNot(_.status ==* Status.Bad)
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
      tt          <- RandomData.tagTree
      (tag, rels) <- RandomData.tagAndRels
    } yield {
      val tt2 = rels.allReferencedIds.foldLeft(tt)((q, id) =>
        q.modOrPut(id, identity, TagInTree(TagId_T.setId(tag, id), Vector.empty)))
      TagProps(tt2, rels, tag)
    }

  val sampleTagTree_f = {
    def t(id: Int, name: String)(children: Int*) = TagInTree(
      ApplicableTag(id, name, s"id=$id", None, ApplicableReqTypes.empty, if (name contains "*") Dead else Live),
      children.toVector.map(ApplicableTagId(_)))
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

  override def tests = Tests {
    import S.Values._

    'PovRelations {
      'derive {
        // Multiple prepend parents, no children
        'v10 - assertEq(derive(v10, S.tagTree), Relations[TagId](
          parents = Map(v1x -> v11, 27.TG -> v11),
          children = Vector.empty))

        // Append parent, no children
        'v13 - assertEq(derive(v13, S.tagTree), Relations[TagId](
          parents = Map(v1x -> None),
          children = Vector.empty))

        // No parents, children
        'status - assertEq(derive(10.TG, S.tagTree), Relations[TagId](
          parents = Map.empty,
          children = Vector(wip, defer, uat, uat2, uat3, prod)))

        // Parents and children
        'released - assertEq(derive(27.TG, S.tagTree), Relations[TagId](
          parents = Map(20.TG -> v1x),
          children = Vector(v09, v10, v11)))
      }
    }

    'flatTagTree {
      def show(rows: Vector[FlatTag]) = rows
        .map(r => s"${"  "*r.depth}${r.tag.name} - ${r.status} - {${r.parentPath.map(_.value).mkString(",")}}")
        .mkString("\n")

      def test(p: FilterPolicy, expect: String): Unit = {
        val a = flatten(sampleTagTree_f)(_.live is Live, p)
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