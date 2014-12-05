package shipreq.webapp.base.data

import shipreq.base.util.TaggedTypes.TaggedLong
import scalaz.std.list._
import shipreq.prop._
import DataImplicits._

object DataProp {

  lazy val rev =
    Prop.test[Rev]("rev ≥ 0", _.value >= 0)

  private def dataSet[O, D, I <: TaggedLong](o: O)(implicit O: ObjDataId[O, D, I]) =
    Prop.distinct("ID", (_: DataSet[D]).data.toStream.map(_.id.value))

  // -------------------------------------------------------------------------------------------------------------------
  // Incompletions

  object customIncmpTypes {
    def all = dataSet(CustomIncmpType)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Requirement Types

  lazy val reqType =
    Prop.test[ReqType]("oldMnemonics doesn't contain current mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

  object customReqType {

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

    lazy val all = (
        dataSet(CustomReqType) ∧ uniqueMnemonics ∧ uniqueNames ∧ rev.contramap(_.rev) ∧ each
      ) rename "customReqTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  lazy val project = (
      customIncmpTypes.all.contramap[Project](_.customIncmpTypes) ∧
      customReqTypes.all.contramap[Project](_.customReqTypes)
    ) rename "Project"
}
