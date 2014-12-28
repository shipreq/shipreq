package shipreq.webapp.base.data

import shipreq.base.util.TaggedTypes.TaggedLong
import scalaz.std.AllInstances._
import shipreq.prop._
import DataImplicits._

object DataProp {

  lazy val rev =
    Prop.test[Rev]("rev ≥ 0", _.value >= 0)

  private def dataSet[O, D, I <: TaggedLong](o: O)(implicit O: ObjDataId[O, D, I]) =
    Prop.distinct("ID", (_: DataSet[D]).data.toStream.map(_.id.value)) ∧ rev.contramap(_.rev)

  // -------------------------------------------------------------------------------------------------------------------
  object customIncmpTypes {
    def all = dataSet(CustomIncmpType) rename "CustomIncmpTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object customReqType {

    lazy val reqType =
      Prop.test[ReqType]("oldMnemonics doesn't contain current mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

    // starting to overlap with validation....
    lazy val mnemonicStatic =
      Prop.test[CustomReqType]("mnemonic doesn't overlap with static",
        a => ReqType.staticMnemonics.intersect(a.oldMnemonics + a.mnemonic).isEmpty)

    lazy val all = mnemonicStatic ∧ reqType.subst
  }

  object customReqTypes {
    type DS = DataSet[CustomReqType]

    lazy val uniqueMnemonics =
      Prop.distinct("mnemonic", (_: DS).data.toStream.flatMap(b => b.mnemonic #:: b.oldMnemonics.toStream).map(_.value))

    lazy val uniqueNames =
      Prop.distinct("name", (_: DS).data.toStream.map(_.name))

    lazy val each =
      customReqType.all.forall[DS, List](_.data)

    lazy val all =
      (dataSet(CustomReqType) ∧ uniqueMnemonics ∧ uniqueNames ∧ each) rename "CustomReqTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  object tags {
    type T = TagTree
    import Tag.Id

    def uniqueNames =
      Prop.distinct("name", (_: T).vstream(_.tag.name))

    def uniqueSiblings =
      Prop.distinctC[Vector, Id]("siblings").forall((_: T).vstream(_.children))

    def noCycles =
      Tag.CycleDetectors.tagTree.noCycleProp("structure")

    def noDeadLinks =
      Prop.subset[T]("ids refers to available tags")(_.keySet, _.vstreamf(_.children.toStream))

    def tagTree =
      (uniqueNames ∧ uniqueSiblings ∧ noCycles ∧ noDeadLinks) rename "TagTree"

    lazy val all =
      (tagTree.contramap[RevAnd[T]](_.data) ∧ rev.contramap(_.rev)) rename "Tags"
  }

  // -------------------------------------------------------------------------------------------------------------------

  lazy val uniqueRefkeys =
    Prop.distinct[Project, RefKey]("refkey", p =>
      p.customIncmpTypes.data.toStream.map(_.key) #:::
      p.tags.data.vstreamf(_.tag.keyO.toStream))

  lazy val project = (
    uniqueRefkeys
      ∧ customIncmpTypes.all.contramap[Project](_.customIncmpTypes)
      ∧ customReqTypes.all.contramap[Project](_.customReqTypes)
      ∧ tags.all.contramap[Project](_.tags)
    ) rename "Project"
}
