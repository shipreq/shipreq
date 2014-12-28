package shipreq.webapp.base.data

import scalaz.Equal
import utest._
import shipreq.prop._
import shipreq.prop.test._
import shipreq.prop.test.PropTest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.UnsafeTypes._
import shipreq.webapp.base.test.{SampleProject => S}
import shipreq.webapp.base.test.BaseTestUtil._
import TagProtocol._
import Tag.Id

object TagsTest extends TestSuite {

  case class PovRelationProp(tt0: TagTree, povRels: PovRelations, t: Tag) {
    val E = EvalOver(this)
    val tt = tt0.add(TagInTree(t, Vector.empty))
    val id = tt.keys.head
    import PovRelations._

    def prop =
      DataProp.tags.tagTree(tt).liftL ==> (
        E.equal("deriveRels(applyRels(r)) = r", derive(id, trustedApply1(povRels, id, tt)), povRels)
      ∧ E.equal("applyRels(deriveRels(t) = t",  trustedApply1(derive(id, tt), id, tt)     , tt)
      )
  }

  val povRelationPropGen: Gen[PovRelationProp] =
    for {
      tt <- RandomData.tagTree
      t  <- RandomData.remoteDeltaG.povTag
    } yield {
      val tt2 = (tt /: t.rels.allReferencedIds)((q, id) =>
        q.modOrPut(id, identity, TagInTree(Tag.IdAccess.setId(t.tag, id), Vector.empty)))
      PovRelationProp(tt2, t.rels, t.tag)
    }

  override def tests = TestSuite {
    'PovRelations {

      'derive {
        // Multiple prepend parents, no children
        "22" - assertEq(PovRelations.derive(22, S.tagTree), PovRelations(
          parents = Map(Id(21) -> Id(23), Id(27) -> Id(23)),
          children = Vector.empty))

        // Append parent, no children
        "24" - assertEq(PovRelations.derive(24, S.tagTree), PovRelations(
          parents = Map(Id(21) -> None),
          children = Vector.empty))

        // No parents, children
        "10" - assertEq(PovRelations.derive(10, S.tagTree), PovRelations(
          parents = Map.empty,
          children = Vector(11, 12)))

        // Parents and children
        "27" - assertEq(PovRelations.derive(27, S.tagTree), PovRelations(
          parents = Map(Id(20) -> Id(21)),
          children = Vector(22, 23)))
      }

      'props - povRelationPropGen.mustSatisfyE(_.prop)
    }
  }
}