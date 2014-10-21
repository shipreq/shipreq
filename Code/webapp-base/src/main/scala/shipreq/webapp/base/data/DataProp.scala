package shipreq.webapp.base.data

import scalaz.std.list._
import shipreq.base.prop._
import DataImplicits._

object DataProp {

  private implicit class SeqExt[A](val s: Seq[A]) extends AnyVal {
    @inline final def isUnique = s.distinct == s
  }

  lazy val rev =
    Prop[Rev]("rev ≥ 0", _.value >= 0)

  private def dataSet[T <: DataAndId : IdAccessor] =
    Prop[DataSet[T]]("each ID is unique", _.data.map(_.id).isUnique)

  // -------------------------------------------------------------------------------------------------------------------
  // Incompletions

  object customIncmpTypes {
    def all = dataSet[CustomIncmpTypeAndId]
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Requirement Types

  lazy val reqType =
    Prop[ReqType]("oldMnemonics doesn't contain current mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

  object customReqType {

    private def mnemonicBlacklist  =
      ReqType.static.map(_.mnemonic).toSet

    // starting to overlap with validation....
    lazy val mnemonicStatic =
      Prop[CustomReqType]("mnemonic doesn't overlap with static",
        a => mnemonicBlacklist.intersect(a.oldMnemonics + a.mnemonic).isEmpty)

    lazy val all = mnemonicStatic ∧ reqType.subst
  }

  object customReqTypes {
    type DS = DataSet[CustomReqTypeAndId]

    lazy val uniqueMnemonics =
      Prop[DS]("each mnemonic is unique",
        _.data.toList.flatMap(b => b.mnemonic :: b.oldMnemonics.toList).isUnique)

    lazy val uniqueNames =
      Prop[DS]("each CustomReqType name is unique", _.data.map(_.name).isUnique)

    lazy val each =
      customReqType.all.forall[DS, List](_.data)

    lazy val all = (
        dataSet[CustomReqTypeAndId] ∧ uniqueMnemonics ∧ uniqueNames ∧ rev.contramap(_.rev) ∧ each
      ) rename "customReqTypes"
  }

  // -------------------------------------------------------------------------------------------------------------------
  lazy val project = (
      customIncmpTypes.all.contramap[Project](_.customIncmpTypes) ∧
      customReqTypes.all.contramap[Project](_.customReqTypes)
    ) rename "Project"
}
